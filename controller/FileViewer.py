import subprocess
import platform


class FileViewer:

    def __init__(self, config):
        self.format = ""
        self.config = config

    def generate(self, path):
        command = "dot -T" + self.format + " " + path + ".dot > " + path + "." + self.format
        subprocess.run(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)

    def show(self, path):
        if platform.system() == "Windows": command = path + "." + self.format
        elif platform.system() == "Darwin": command = "open " + path + "." + self.format
        subprocess.run(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
