# Makefile

bot: *.lisp
	sbcl --script MyBot.lisp;

submission-zip: *.lisp
	( rm -f my_submission.zip ; zip -r my_submission.zip *.lisp; )

clean:
	rm -f MyBot my_submission.zip *.log;
