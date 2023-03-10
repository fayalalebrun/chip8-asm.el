;;; chip8-asm.el --- CHIP-8 Assembler -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Francisco Ayala Le Brun <frankxlebrun@gmail.com>

;; Author: Francisco Ayala Le Brun <frankxlebrun@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") dash)
;; Keywords: tools

;; This file is not part of GNU Emacs.

(rx-define chip8-spc (+ whitespace)) ; Spacing
(rx-define chip8-wss (x) (seq bol (* whitespace) x (* whitespace) eol)) ; Whitespace sandwich
(rx-define chip8-v (x) (seq "V" (group x) (opt ","))) ; V followed by pattern x
(rx-define chip8-vx (chip8-v hex)) ; V register parameter
(rx-define chip8-nwt (+ (not whitespace))) ; Non-whitespace token group
(rx-define chip8-nwtg (group chip8-nwt)) ; Non-whitespace token

;; Parses a number but returns an error if it exceeds max
(defun chip8-asm-parse-number (number &optional max)
  (-let [parsed (pcase number
		  ((rx (chip8-wss (seq "%" (group (one-or-more (or "0" "1"))))))
		   (string-to-number (match-string 1 number) 2))
		  ((rx (chip8-wss (seq "$" (group (one-or-more hex)))))
		   (string-to-number (match-string 1 number) 16))
		  ((rx (chip8-wss (seq (group (one-or-more digit)))))
		   (string-to-number (match-string 1 number) 10))
		  (_ (error "Could not parse as number: %s" number))
		  )]
    (if (and (not (null max)) (< max parsed))
	(error "Number %i larger than max allowed %i" parsed max)
      parsed
      ))
  )

(defun chip8-asm-op (pos0 pos1 pos2 pos3)
  (let ((x (+ (ash pos0 12) (ash pos1 8) (ash pos2 4) pos3)))
    (list (logand #xFF (ash x -8)) (logand `#xFF x)))
  )

;; Parses an instruction which takes an addr, with a given prefix
;; Produces a function which given the label map, produces the final instruction
(defun chip8-asm-parse-addr (prefix match identity)
  (pcase match
    ((and (rx  (chip8-wss (group (one-or-more (or "$" "%" hex)))))
	  (app (lambda (x) (chip8-asm-parse-number (match-string 1 x))) (and val (guard (<= 0 val #xFFF)))))
     (funcall identity (chip8-asm-op prefix #x0 #x0 val)))

    ((and val (rx (group (one-or-more (or alphanumeric "-" "_")))))
     (lambda (label-assoc) (chip8-asm-op prefix #x0 #x0
					 (-let [addr (alist-get val label-assoc nil nil 'string-equal)]
					   (if addr addr (error "Label not found: %s" val)))))))
  )

;; Parses a byte given a register, byte and prefix, creating an instruction
(defun chip8-asm-parse-byte (prefix register byte identity)
  (let ((register (string-to-number register 16))
	(byte (chip8-asm-parse-number byte)))
    (when (and (<= 0 register #xF) (<= 0 byte #xFF))
      (funcall identity (chip8-asm-op prefix register #x0 byte)))
    )
  )

;; Parse an instruction which takes two registers
(defun chip8-asm-parse-two-reg (prefix reg1 reg2 suffix identity)
  (funcall identity (chip8-asm-op prefix (string-to-number reg1 16) (string-to-number reg2 16) suffix))
  )

(defun chip8-asm-parse-two-reg-and-nibble (prefix reg1 reg2 nibble identity)
  (let ((nibble (chip8-asm-parse-number nibble)))
    (if (<= #x0 nibble #xF)
	(funcall identity (chip8-asm-op prefix (string-to-number reg1 16) (string-to-number reg2 16) nibble))
      (error "Nibble out of range")
      )
    )
  )

					; we aim to encode each instruction so that given a map of labels to locations, we can
					; produce the assembled instruction
(defun chip8-asm-parse-expr (expr)
  (let ((identity (lambda (instr) (lambda (label-assoc) instr)))) ; identity lambda returns the instruction as-is
    (pcase expr
      ;; SUPER Chip-48
      ((rx (chip8-wss (seq "SCD" chip8-spc  chip8-nwtg)))
       (chip8-asm-parse-two-reg-and-nibble #x0 "0" "C" (match-string 1 expr) identity))
      ((rx (chip8-wss "SCR"))
       (funcall identity (chip8-asm-op #x0 #x0 #xF #xB)))
      ((rx (chip8-wss "SCL"))
       (funcall identity (chip8-asm-op #x0 #x0 #xF #xC)))
      ((rx (chip8-wss "EXIT"))
       (funcall identity (chip8-asm-op #x0 #x0 #xF #xD)))
      ((rx (chip8-wss "LOW"))
       (funcall identity (chip8-asm-op #x0 #x0 #xF #xE)))
      ((rx (chip8-wss "HIGH"))
       (funcall identity (chip8-asm-op #x0 #x0 #xF #xF)))
      ((rx (chip8-wss (seq "DRW" chip8-spc chip8-vx chip8-spc chip8-vx chip8-spc "0")))
       (chip8-asm-parse-two-reg #xD (match-string 1 expr) (match-string 2 expr) #x0 identity))
      ((rx (chip8-wss (seq "LD" chip8-spc "HF" (opt ",") chip8-spc chip8-vx)))
       (chip8-asm-parse-two-reg #xF (match-string 1 expr) "3" #x0 identity))
      ((rx (chip8-wss (seq "LD" chip8-spc "R" (opt ",") chip8-spc chip8-vx)))
       (chip8-asm-parse-two-reg #xF (match-string 1 expr) "7" #x5 identity))
      ((rx (chip8-wss (seq "LD" chip8-spc chip8-vx chip8-spc "R")))
       (chip8-asm-parse-two-reg #xF (match-string 1 expr) "8" #x5 identity))
      ;; Chip-8
      ((rx (chip8-wss "CLS"))
       (funcall identity (chip8-asm-op #x0 #x0 #xE #x0)))
      ((rx (chip8-wss "RET"))
       (funcall identity (chip8-asm-op #x0 #x0 #xE #xE)))
      ((rx (chip8-wss (seq "SYS" chip8-spc chip8-nwtg)))
       (chip8-asm-parse-addr #x0 (match-string 1 expr) identity))
      ((rx (chip8-wss (seq "JP" chip8-spc (chip8-v "0") chip8-spc chip8-nwtg)))
       (chip8-asm-parse-addr #xB (match-string 2 expr) identity))
      ((rx (chip8-wss (seq "JP" chip8-spc chip8-nwtg)))
       (chip8-asm-parse-addr #x1 (match-string 1 expr) identity))
      ((rx (chip8-wss (seq "CALL" chip8-spc chip8-nwtg)))
       (chip8-asm-parse-addr #x2 (match-string 1 expr) identity))
      ((rx (chip8-wss (seq "SE" chip8-spc chip8-vx chip8-spc chip8-vx)))
       (chip8-asm-parse-two-reg #x5 (match-string 1 expr) (match-string 2 expr) #x0 identity))
      ((rx (chip8-wss (seq "SNE" chip8-spc chip8-vx chip8-spc chip8-vx)))
       (chip8-asm-parse-two-reg #x9 (match-string 1 expr) (match-string 2 expr) #x0 identity))
      ((rx (chip8-wss (seq "SE" chip8-spc chip8-vx chip8-spc chip8-nwtg)))
       (chip8-asm-parse-byte #x3 (match-string 1 expr) (match-string 2 expr) identity))
      ((rx (chip8-wss (seq "SNE" chip8-spc chip8-vx chip8-spc chip8-nwtg)))
       (chip8-asm-parse-byte #x4 (match-string 1 expr) (match-string 2 expr) identity))
      ((rx (chip8-wss (seq "ADD" chip8-spc chip8-vx chip8-spc chip8-vx)))
       (chip8-asm-parse-two-reg #x8 (match-string 1 expr) (match-string 2 expr) #x4 identity))
      ((rx (chip8-wss (seq "ADD" chip8-spc chip8-vx chip8-spc  chip8-nwtg)))
       (chip8-asm-parse-byte #x7 (match-string 1 expr) (match-string 2 expr) identity))
      ((rx (chip8-wss (seq "LD" chip8-spc chip8-vx chip8-spc chip8-vx)))
       (chip8-asm-parse-two-reg #x8 (match-string 1 expr) (match-string 2 expr) #x0 identity))
      ((rx (chip8-wss (seq "OR" chip8-spc chip8-vx chip8-spc chip8-vx)))
       (chip8-asm-parse-two-reg #x8 (match-string 1 expr) (match-string 2 expr) #x1 identity))
      ((rx (chip8-wss (seq "AND" chip8-spc chip8-vx chip8-spc chip8-vx)))
       (chip8-asm-parse-two-reg #x8 (match-string 1 expr) (match-string 2 expr) #x2 identity))
      ((rx (chip8-wss (seq "XOR" chip8-spc chip8-vx chip8-spc chip8-vx)))
       (chip8-asm-parse-two-reg #x8 (match-string 1 expr) (match-string 2 expr) #x3 identity))
      ((rx (chip8-wss (seq "SUB" chip8-spc chip8-vx chip8-spc chip8-vx)))
       (chip8-asm-parse-two-reg #x8 (match-string 1 expr) (match-string 2 expr) #x5 identity))
      ((rx (chip8-wss (seq "SHR" chip8-spc chip8-vx chip8-spc chip8-vx)))
       (chip8-asm-parse-two-reg #x8 (match-string 1 expr) (match-string 2 expr) #x6 identity))
      ((rx (chip8-wss (seq "SUBN" chip8-spc chip8-vx chip8-spc chip8-vx)))
       (chip8-asm-parse-two-reg #x8 (match-string 1 expr) (match-string 2 expr) #x7 identity))
      ((rx (chip8-wss (seq "SHL" chip8-spc chip8-vx chip8-spc chip8-vx)))
       (chip8-asm-parse-two-reg #x8 (match-string 1 expr) (match-string 2 expr) #xE identity))
      ((rx (chip8-wss (seq "LD" chip8-spc "I" (opt ",") chip8-spc chip8-nwtg)))
       (chip8-asm-parse-addr #xA (match-string 1 expr) identity))
      ((rx (chip8-wss (seq "RND" chip8-spc chip8-vx chip8-spc  chip8-nwtg)))
       (chip8-asm-parse-byte #xC (match-string 1 expr) (match-string 2 expr) identity))
      ((rx (chip8-wss (seq "DRW" chip8-spc chip8-vx chip8-spc chip8-vx chip8-spc chip8-nwtg)))
       (chip8-asm-parse-two-reg-and-nibble #xD (match-string 1 expr) (match-string 2 expr) (match-string 3 expr) identity))
      ((rx (chip8-wss (seq "SKP" chip8-spc chip8-vx)))
       (chip8-asm-parse-two-reg #xE (match-string 1 expr) "9" #xE identity))
      ((rx (chip8-wss (seq "SKNP" chip8-spc chip8-vx)))
       (chip8-asm-parse-two-reg #xE (match-string 1 expr) "A" #x1 identity))
      ((rx (chip8-wss (seq "LD" chip8-spc chip8-vx chip8-spc "DT")))
       (chip8-asm-parse-two-reg #xF (match-string 1 expr) "0" #x7 identity))
      ((rx (chip8-wss (seq "LD" chip8-spc chip8-vx chip8-spc "K")))
       (chip8-asm-parse-two-reg #xF (match-string 1 expr) "0" #xA identity))
      ((rx (chip8-wss (seq "LD" chip8-spc "DT" (opt ",") chip8-spc chip8-vx)))
       (chip8-asm-parse-two-reg #xF (match-string 1 expr) "1" #x5 identity))
      ((rx (chip8-wss (seq "LD" chip8-spc "ST" (opt ",") chip8-spc chip8-vx)))
       (chip8-asm-parse-two-reg #xF (match-string 1 expr) "1" #x8 identity))
      ((rx (chip8-wss (seq "ADD" chip8-spc "I" (opt ",") chip8-spc chip8-vx)))
       (chip8-asm-parse-two-reg #xF (match-string 1 expr) "1" #xE identity))
      ((rx (chip8-wss (seq "LD" chip8-spc "F" (opt ",") chip8-spc chip8-vx)))
       (chip8-asm-parse-two-reg #xF (match-string 1 expr) "2" #x9 identity))
      ((rx (chip8-wss (seq "LD" chip8-spc "B" (opt ",") chip8-spc chip8-vx)))
       (chip8-asm-parse-two-reg #xF (match-string 1 expr) "3" #x3 identity))
      ((rx (chip8-wss (seq "LD" chip8-spc "[I]" (opt ",") chip8-spc chip8-vx)))
       (chip8-asm-parse-two-reg #xF (match-string 1 expr) "5" #x5 identity))
      ((rx (chip8-wss (seq "LD" chip8-spc chip8-vx chip8-spc "[I]")))
       (chip8-asm-parse-two-reg #xF (match-string 1 expr) "6" #x5 identity))
      ((rx (chip8-wss (seq "LD" chip8-spc chip8-vx chip8-spc  chip8-nwtg)))
       (chip8-asm-parse-byte #x6 (match-string 1 expr) (match-string 2 expr) identity))
      (_ (error "Unknown instruction: %s" expr)
	 ))))

					; Parses line recursively to parse bytes separated by spaces
(defun chip8-asm-parse-data-byte (line)
  (pcase line
    ((rx (chip8-wss (seq  chip8-nwtg (group (* (seq chip8-spc chip8-nwt))))))
     (let ((number (match-string 1 line)) (rest (match-string 2 line)))
       (cons (chip8-asm-parse-number number #xFF) (chip8-asm-parse-data-byte rest))))
    ((rx bol (opt chip8-spc) eol)
     nil)
    (_ (error "Could not parse data byte: %s" line)))
  )

;; Parses line and return number of bytes this line increases and function which returns bytes
(defun chip8-asm-parse-instr-or-data (line)
  (pcase line
    ((rx (chip8-wss (seq "db" (+ whitespace) (group (* anychar))))) ; db can be used in order to insert data directly
     (-let [bytes (chip8-asm-parse-data-byte (match-string 1 line))]
       (list (length bytes) (lambda (label-assoc) bytes)))) ; measure the amount of data
    (_ (list 2 (chip8-asm-parse-expr line))) ; instructions always 2 bytes wide
    )
  )

;; Gives the next address, the label association list and the data lambda association list
;; The label association list maps addresses to labels
;; The data lambda association list maps addresses to a lambda which produces some bytes
;; when given the label association list
(defun chip8-asm-parse-label-and-instr (line line-num curr-addr label-assoc data-assoc)
  (condition-case err

      (-let [line (substring line 0 (string-match ";" line))] ; Remove possible comment from line
	(-let [(label byte-num get-data)
	       (pcase line
		 ((rx (chip8-wss ""))
		  (list nil 0 nil))
		 ((rx (chip8-wss (seq "ORG" chip8-spc chip8-nwtg))) ; Tells us location to place following bytes
		  (list nil (- (chip8-asm-parse-number (match-string 1 line)) curr-addr) nil))
		 ((rx bol (group (+ (or alphanumeric "-" "_"))) ":" (opt chip8-spc) eol) ; With label
		  (list (match-string 1 line) 0 nil))
		 ((rx bol (group (+ (or alphanumeric "-" "_"))) ":" (group (* anychar)) eol) ; With label
		  (cons (match-string 1 line) (chip8-asm-parse-instr-or-data (match-string 2 line))))
		 ((rx bol (* anychar) chip8-nwt (* anychar)) ; No label
		  (cons nil (chip8-asm-parse-instr-or-data line)))
		 )]
	  (list (+ curr-addr byte-num)
		(if (not (null label)) (push (cons label curr-addr) label-assoc) label-assoc)
		(if (not (null get-data)) (push (cons curr-addr get-data) data-assoc) data-assoc))
	  )
	)
    (error (error "Line %i: %s" line-num err)))
  )



(defun chip8-asm-parse (lines)
  
  (-let (((_ label-assoc data-assoc) ; First, get the addresses of the labels and instructions
	  (->> lines
	       (-map-indexed (lambda (index item) (cons (+ index 1) item)))
	       (-reduce-from
		(lambda (acc it)
		  (-let (((curr-addr label-assoc data-assoc) acc)
			 ((line-num . line) it))
		    (chip8-asm-parse-label-and-instr line line-num curr-addr label-assoc data-assoc)))
		(list 0 nil nil)
		)))
	 (rom (make-vector #x1000 0)))
    (-each data-assoc ; Now we can create the instructions with the label addresses
      (lambda (data-func) (-let [(addr . data-func) data-func]
			    (-each-indexed ; Need to iterate through the return bytes to set each of them
				(funcall data-func label-assoc)
			      (lambda (index byte) (aset rom (+ addr index) byte))))))
    (cons rom (car (car (last data-assoc)))) ; The last element of the list contains the first address
    )
  )

(defun chip8-asm (path)
  (interactive "F")
  (-let [lines (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n")]
    (-let [(bytes . initial-offset) (chip8-asm-parse lines)]
      (with-temp-file path
	(progn
	  (erase-buffer)
	  (set-buffer-multibyte nil)
	  (goto-char (point-min))
	  (-each
	      (-drop initial-offset (append bytes nil))
	    (lambda (b) (insert b)))))
      )))

(defun chip8-run-buffer ()
  (interactive)
  (chip8-asm "/tmp/emacs-chip8.bin")
  (chip8-emulate "/tmp/emacs-chip8.bin"))

