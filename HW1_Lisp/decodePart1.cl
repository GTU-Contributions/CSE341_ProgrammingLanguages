; *********************************************
; *  341  Programming Languages               *
; *  Fall 2016                                *
; *  Author: Liu Liu                          *
; *          Ulrich Kremer                    *
; *          Furkan Tektas , clisp            *
; *********************************************

;; MEHMED_MUSTAFA_131044084 


;; ENVIRONMENT
;; "c2i, "i2c",and "apply-list"
(load "include.cl")

;; test document
(load "document.cl")

;; test-dictionary
;; this is needed for spell checking
;;(load "test-dictionary.cl")

(load "dictionary.cl") ;;  real dictionary (45K words)

;; -----------------------------------------------------
;; HELPERS

;; Parameters: The word to be searched; The list to be searched in;
;; Return: true if "word" exist in "list_to_search"
(defun isInList (word list_to_search)
	(if (not (null list_to_search))
		(dolist (n list_to_search)
			(if (equal word n)(return-from isInList t)))
		nil
	)
)

;; Parameters: The word to be searched;
;; Returns: true if "word" exist in the dictionary
(defun spell_checker (word) (isInList word *dictionary*))

;; Parameters: The list which will be sorted
;; Returns: Sorted list according to the sublist's lenght starting from the biggest
;; THIS FUNCTION IS USED ONLY FOR FAST TESTING WHICH IS CHOOSABLE IN CODE-BREAKER FUNCTION
(defun lenght_sort (list_to_sort)
	(map 'list (function cdr)
		(sort (map 'vector (lambda (list) (cons (length list) list)) list_to_sort)
			(function >) :key (function car)))
)

;; Parameters: The alphabet list where the last changed flag will be find
;; Returs: The number of the last changed flag
(defun findLastChangedCharFlag (alphabetList)
	(let ((maxNumber0 0))

	(dolist (n alphabetList)
		(if (< maxNumber0 (caddr n)) (setf maxNumber0 (caddr n)))
	)

	(return-from findLastChangedCharFlag maxNumber0))
)

;; Parameters: The alphabet list where the testChar will be checked
;; Returns: true if the testChar is mapped to some letter
(defun is_char_mapped (testChar alphabetList)
	(dolist (n alphabetList)
		(if (equal testChar (cadr n))
			(return-from is_char_mapped t)
		)
	)

	(return-from is_char_mapped nil)
)

;; Parameters: The alphabet list where the mapping will be done
;; Returns: true if the charToPlace is successfully mapped to the sourceChar 
(defun map_char (sourceChar charToPlace alphabetList)
	(let ((maxNumber (findLastChangedCharFlag alphabetList)))

	(dolist (n alphabetList)
		(if (and (equal sourceChar (car n))(equal '_ (cadr n)))
			(progn 
				(setf (cadr n) charToPlace)
				(setf (caddr n) (+ maxNumber 1))
				(return-from map_char t)
			)
		)
	)

	(return-from map_char nil))
)

;; Parameters: The alphabet list where every mapped char will be unmapped if it's flag is bigger than parameter flag
(defun unmap_chars (flag alphabetList)
	(dolist (n alphabetList)
		(if (< flag (caddr n))
			(progn
				(setf (caddr n) 0)
				(setf (cadr n) '_)
			)

		)
	)
)

;; Parameters: The alphabet list where the mapped char will be returned
;; Returns: The mapped char of originalChar
(defun get_mapped_char (originalChar alphabetList)
	(dolist (n alphabetList)
		(if (equal originalChar (car n))
			(return-from get_mapped_char (cadr n)))))

;; Parameters: The alphabet list where the original char will be returned
;; Returns: The original char of mappedChar
(defun get_original_char (mappedChar alphabetList)
	(dolist (n alphabetList)
		(if (equal mappedChar (cadr n))
			(return-from get_original_char (car n)))))

;; Parameters: The alphabet list where the mapping will be done, encoded word and dictionary word
;; Returns: true if the mapping of the word is successful
(defun map_word (encoded_word dic_word alphabetList)
	(let ((maxNumber2 (findLastChangedCharFlag alphabetList)))
	;;(format t "---------------------------------------------------------------------------")
	;;(format t "maxNumber2: ~a~%" maxNumber2)

	(dolist (n encoded_word)

		;;(format t "n: ~a~%" n)
		(setf firstChar (car dic_word))
		;;(format t "firstChar of DIC_WORD: ~a~%" firstChar)
		(setf dic_word (cdr dic_word))
		;;(format t "REST OF dic_word: ~a~%" dic_word)
		(setf value (get_mapped_char n alphabetList))
		;;(format t "value: ~a~%" value)

		(if(and (is_char_mapped firstChar alphabetList)
		   (not (equal (get_original_char firstChar alphabetList) n)))
			(progn
				(unmap_chars maxNumber2 alphabetList)
				(return-from map_word nil)	
			)	
		)

		(cond
			((equal value '_) (map_char n firstChar alphabetList))
			((equal value firstChar) (continue))
			(t  (progn
					(unmap_chars maxNumber2 alphabetList)
					(return-from map_word nil)
				)
			)
		)
		;;(format t "~a~%~%" alphabetList)
	)

	(return-from map_word t))
)

;; Parameters: The paragraph which will be scanned and the alphabetlist where the values will be saved
;; Returns: true if the mapping pattern is found
(defun getMappedAlphabet (paragraph alphabetList)

	(let ((paragraph_head (car paragraph)) (paragraph_rest (cdr paragraph)))
	;;(format t "---PARA_HEAD: ~a~%" paragraph_head)
	;;(format t "---PARA_REST: ~a~%" paragraph_rest)

	(if (null paragraph_head) (return-from getMappedAlphabet t))

	(let ((maxNumber3 (findLastChangedCharFlag alphabetList)))
	;;(format t "maxNumber3: ~a~%" maxNumber3)

	(dolist (dic_word *dictionary*)
		(if (equal (length paragraph_head)(length dic_word))
			(progn
				(if (map_word paragraph_head dic_word alphabetList)
					(progn
						;;(format t "__________DIC_WORD: ~a~%" dic_word)
						(if(getMappedAlphabet paragraph_rest alphabetList)
							(return-from getMappedAlphabet t)
						)
					)
					(progn
						(unmap_chars maxNumber3 alphabetList)
						(continue)
					)
				)
			)
		)
	)

	;;(format t "End of the function! HEAD: ~a~%~%" paragraph_head)
	(return-from getMappedAlphabet nil)
	))
)

;; -----------------------------------------------------
;; DECODE HELPERS

(defun getPlainWord (encoded-word mappedAlphabet)
	(cond 
		((null encoded-word) '())
		(T (append (list (get_mapped_char (car encoded-word) mappedAlphabet))
				   (getPlainWord (cdr encoded-word) mappedAlphabet)))))
(defun getPlainParagraph (encoded-paragraph mappedAlphabet)
	(cond 
		((null encoded-paragraph) '())
		(T (cons (getPlainWord (car encoded-paragraph) mappedAlphabet)
				   (getPlainParagraph (cdr encoded-paragraph) mappedAlphabet)))))
(defun getPlainDocument (encoded-document mappedAlphabet)
	(cond 
		((null encoded-document) '())
		(T (cons (getPlainParagraph (car encoded-document) mappedAlphabet)
				   (getPlainDocument (cdr encoded-document) mappedAlphabet)))))
(defun transform (document)
	(cond 
		((null document) '())
		(T (append (car document)
				 (transform (cdr document))))))

;; -----------------------------------------------------
;; ENCODE FUNCTIONS

;; THESE ENCODING FUNCTIONS ARE FROM INTERNET JUST FOR TESTING PURPOSE AND CHANGED A LITTLE BIT TO WORK IN LISP 
(defun encode_word (n word)
	(map 'list #'i2c 
		(map 'list #'(lambda (m) (mod (+ m n) 26)) 
			(map 'list #'c2i word))))
(defun encode_paragraph (n paragraph)
	(cond 
		((null paragraph) '())
		(T (cons (encode_word n (car paragraph)) 
				 (encode_paragraph n (cdr paragraph))))))
(defun encode_document (n document)
	(cond 
		((null document) '())
		(T (cons (encode_paragraph n (car document))
				 (encode_document n (cdr document))
			)
		)
	))

;; -----------------------------------------------------
;; DECODE FUNCTIONS

(defun Gen-Decoder-A (paragraph)
	;; INITIALIZE ALPHABET WHERE FIRST ELEMENT IS THE ALPHABET 
	;; THE SECOND ELEMENT WILL BE THE MAPPED ELEMENT
	;; THE THIRD ELEMENT WILL BE THE FLAG TO KNOW WHICH LETTER WAS LAST CHANGED
	(let ((*alphabet* '(
		(a _ 0)(b _ 0)(c _ 0)(d _ 0)(e _ 0)(f _ 0)(g _ 0)(h _ 0)(i _ 0)(j _ 0)(k _ 0)(l _ 0)(m _ 0)
		(n _ 0)(o _ 0)(p _ 0)(q _ 0)(r _ 0)(s _ 0)(t _ 0)(u _ 0)(v _ 0)(w _ 0)(x _ 0)(y _ 0)(z _ 0)
		)))

	;; FINDS ALL MAPPED LETTERS INSIDE A PARAGRAPH
	(getMappedAlphabet paragraph *alphabet*)

	(return-from Gen-Decoder-A *alphabet*))
)

(defun Gen-Decoder-B-0 (paragraph)
  ;you should implement this function
)

(defun Gen-Decoder-B-1 (paragraph)
  ;you should implement this function
)

(defun Code-Breaker (document decoder-function)

	;; FOR FAST TESTING I HAVE WROTE lenght_sort method to sort from longest word to the shortest

	;; FOR FASTEST RUN TIME USE THE LINE AFTER THIS AND REMOVE THE FIRST CODE LINE
	;; (let ((docParagraph (lenght_sort (transform document))))

	;; THIS IS THE SLOWER METHOD
	(let ((docParagraph (transform document)))

	;; GETS THE MAPPING LETTER PATTERNS
	(let ((mappedAlphabet (funcall decoder-function docParagraph)))

	;; RETURNS PLAIN TEXT
	(return-from Code-Breaker (getPlainDocument document mappedAlphabet))))
)

;; ----------------------------------------- TESTING ----------------------------------------- ;;

(format t "_________________ENCODED DOCUMENT_________________~%")

;; TEST DOCUMENT
(defparameter *document* '(
                   ((t h i s)(c o u r s e)(c o v e r s)(t o p i c s)(i n)(p r o g r a m m i n g)(l a n g u a g e s)(a n d)(c o m p i l e r s))
                   ((a t t r i b u t e)(g r a m m a r s)(a n d)(t h e i r)(u s e)(i n)(s y n t a x)(d i r e c t e d)(t r a n s l a t i o n))
                   ((m o d e l s)(o f)(p r o g r a m m i n g)(l a n g u a g e)(s e m a n t i c s))
                   ((i n t e r m e d i a t e)(r e p r e s e n t a t i o n s)(o f)(p r o g r a m s)(p a r a l l e l)(p r o g r a m m i n g)(m o d e l s))
                   ))

;; ENCODES THE DOCUMENT SHIFTING THE ALPHABET BY 5 
;; THIS CODE IMPLEMENTATIN WORKS EVEN WITH RANDOM MAPPING
;; I HAVE USED THIS SHIFTING ENCODING SYSTEM JUST FOR FAST TESTING
(defparameter *encoded-document-test* (encode_document 5 *document*)) 

;; PRINT THE ENCODED DOCUMENT
(format t "~a~%~%" *encoded-document-test*)
;;(format t "~a~%~%" (lenght_sort (transform *encoded-document-test*)))
(format t "_________________CALCULATING_________________~%")
(format t "~a~%~%" (Code-Breaker *encoded-document-test* #'Gen-Decoder-A))
