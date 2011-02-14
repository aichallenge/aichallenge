import os.path
import worker.language
from worker.runner import Runner

# statuses
UNCOMPILED, COMPILE_FAILED, UNTESTED, TEST_FAILED, READY = range(5)

short_messages = {
    UNCOMPILED : "has not been compiled.",
    COMPILE_FAILED : "failed to compile.",
    UNTESTED : "has not been tested.",
    TEST_FAILED : "failed one or more test cases.",
    READY : "is ready for battle!",
}

long_messages = {
    UNCOMPILED :
            ("Unfortunately, your latest submission to the Google AI "
             "Challenge was not compiled for some reason. Please check "
             "the error messages below for more information."),
    COMPILE_FAILED : 
            ("Unfortunately, your latest submission to the Google AI "
             "Challenge did not compile successfully. Please check the "
             "error messages below for more information. Fix as many of "
             "the errors as you can, then submit your code again."),
    UNTESTED :
            ("Unfortunately, your latest submission to the Google AI "
             "Challenge was not tested for some reason, though it appears "
             "to have compiled successfully. Please check the error messages "
             "below for more information."),
    TEST_FAILED : 
            ("Unfortunately, your latest submission to the Google AI "
             "Challenge did not pass all of the submission tests. Please "
             "check the error messages below for more information. Fix as "
             "many of the errors as you can, then submit your code again."),
    READY :
            ("This is just to let you know that your latest submission "
             "to the Google AI Challenge has successfully compiled and "
             "passed all of the submission tests. Here is the output of "
             "the compile script, in case you're curious:"),
}

# Potential subject lines for emails/messages
subjects = {
    UNCOMPILED : "Submission Error!",
    COMPILE_FAILED : "Compile Failure!",
    UNTESTED : "Submission Error!",
    TEST_FAILED : "Test Failure!",
    READY : "Submission Success!",
}

class Submission(object):
    """ Encapsulates a submission while the backend is processing it. """
    def __init__(self, username, sub_id, directory,
                 language=None, status=UNCOMPILED):
        self.username = username
        self.sub_id = sub_id
        self.directory = directory
        self.language = language
        self.status = status
        # Main executable/interpretable
        self.bot = None
        self.compile_output = ""
        self.compile_errors = ""
        self.test_results = ""
        
        if self.language is None:
            languages = list(worker.language.detect_languages(self))
            if len(languages) == 1:
                self.language, main_file = languages[0]

    def summary(self):
        return "Submission %d %s" % (self.sub_id, short_messages[self.status])
    
    def full_report(self):
        """ Generate a report of what happened to the submission, with
            compilation details (and test results if necessary), with the
            intent of being delivered to the submitter. """
        s = "%s,\n\nYour submission (id: %d) " % (self.username, self.sub_id)
        s += short_messages[self.status] + "\n\n"
        s += long_messages[self.status] + "\n\n"
        if self.status == TEST_FAILED:
            s += "TEST RESULTS\n\n" + self.test_results + "\n\n"
        # Always append these
        s += "COMPILE OUTPUT\n\n" + self.compile_output + "\n\n"
        s += "COMPILE ERRORS\n\n" + self.compile_errors + "\n\n"
        s += "Sincerely,\nThe Compile Script"
        return s
    
    def get_command(self, directory):
        """return the command needed to run the bot"""
        return worker.language.get_command(self, directory)

    def compile(self, max_time=300):
        """ Determines which language this submission is coded in, and
            compiles it. Optionally, a time limit may be specified to prevent
            overlong compilation times. """
        assert self.status == UNCOMPILED # don't attempt multiple compilations
        if worker.language.compile_submission(self, max_time):
            self.status = UNTESTED # compile success
            return True
        else:
            self.status = COMPILE_FAILED
            return False
    
    def run(self):
        """Launches the submission inside a runner so that it may be used in
            a game."""
        runner = Runner(origin=self.directory)
        runner.run(self.get_command(runner.working))
        return runner
    
    def compile_if_needed(self, max_time=300, logger=None):
        if os.path.exists(self.directory + "/.aichallenge/compiled"):
            self.status = UNTESTED
            return True
        else:
            if logger is not None:
                logger.info('compiling %s' % self.directory)
            return self.compile(max_time)
