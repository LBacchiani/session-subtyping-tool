import subprocess
import platform


class FileViewer:

    def __init__(self, config):
        self.format = ""
        self.config = config

    def generate(self, path, dotname, imgname):
        command = "dot -T" + self.format + " " + path + dotname + ".dot > " + path + imgname + "." + self.format
        subprocess.run(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)

    def show(self,  path, name):
        command = path + name + "." + self.format
        if platform.system() == "Darwin": command = "open " + command
        subprocess.run(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
