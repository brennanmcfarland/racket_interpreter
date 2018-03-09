; Brennan McFarland
; Lucas Alva

(require racket/trace)

; run all tests
(define test
  (lambda ()
    (test_all)))

; run all tests
(define test_all
  (lambda ()
    (test_case 1 20)))

; run up to max numbered tests
(define test_first
  (lambda (maxtst)
    (test_case 1 maxtst)))

; run tests in range
(define test_range
  (lambda (mintst maxtst)
    (test_case mintst maxtst)))

(define test_case
  (lambda (testnum maxtst)
    (cond
      ((> testnum maxtst) "all tests passed")
      ((= testnum 11) (test_case 15 maxtst))
      ((not (eq? (interpret (string-append "test_cases/test" (string-append (number->string testnum) ".html"))) (interpret (string-append "test_cases/test" (string-append (number->string testnum) ".out"))))) (error (string-append "test " (string-append (number->string testnum) " failed"))))
      (else (test_case (+ testnum 1) maxtst)))))
