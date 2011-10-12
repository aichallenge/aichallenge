import markdown
import re

START_FENCE = re.compile('^```(.*)')
END_FENCE = re.compile('^```$')

class GithubPreprocessor(markdown.preprocessors.Preprocessor):
    def run(self, lines):
        # replace github fenced code blocks with python markdown ones
        new_lines = []
        fenced = False
        for line in lines:
            if fenced:
                if END_FENCE.match(line):
                    new_lines.append('')
                    fenced = False
                else:
                    new_lines.append('    '+line)
            else:
                fence = START_FENCE.match(line)
                if fence:
                    fenced = True
                    new_lines.append('')
                    if fence.group(1):
                        new_lines.append('    :::' + fence.group(1))
                else:
                    new_lines.append(line)
        return new_lines

class MarkdownGithub(markdown.Extension):
    def extendMarkdown(self, md, md_globals):
        md.preprocessors.add('github',
                GithubPreprocessor(self), "_begin")

def makeExtension(configs=None):
    return MarkdownGithub(configs=configs)
