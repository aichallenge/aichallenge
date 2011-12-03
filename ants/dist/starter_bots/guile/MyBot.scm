(include "ants.scm")

(define ai (make <ai>))
(ai:setup ai
	  (lambda (ai)
	    ;; your setup code here, if any
	    #t))
(ai:run ai 
	(lambda (ai)
	  ;; your turn code here
	  
	  (for-each (lambda (ant)
		      (for-each (lambda (dir)
				  (if (square:land? 
				       (square:neighbor (ant:square ant) dir))
				      (ant:order ant dir)
				      ));; end (lambda (dir
				'(N E S W)) ;; end for-each
		      );; end (lambda (ant
		    (ai:my-ants ai)
		    ) ;; end for-each
	  ));; end ai:run



