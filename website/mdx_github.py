import markdown
import re

FENCE = re.compile('^```+(.*)')

class GithubPreprocessor(markdown.preprocessors.Preprocessor):
    def run(self, lines):
        # replace github fenced code blocks with python markdown ones
        for i, line in enumerate(lines):
            fence = FENCE.match(line)
            if fence:
                if fence.group(1):
                    lines[i] = '~~~~{.' + fence.group(1) + '}'
                else:
                    lines[i] = '~~~~'
        return lines

class MarkdownGithub(markdown.Extension):
    def extendMarkdown(self, md, md_globals):
        md.preprocessors.add('github',
                GithubPreprocessor(self), "_begin")

def makeExtension(configs=None):
    return MarkdownGithub(configs=configs)
