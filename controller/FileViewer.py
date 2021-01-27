import subprocess
import platform
import json

class FileViewer:

    def __init__(self, config):
        self.format = ""
        self.config = config
        self.general_config = json.load(open("general_config.json"))

    def generate(self, path, dotname, imgname):
        command = "dot -T" + self.format + " " + path + dotname + ".dot > " + path + imgname + "." + self.format
        out = str(subprocess.run(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT).stdout)
        return out if len(out) > 3 else ""

    def show(self,  path, name):
        command = path + name + "." + self.format
        if platform.system() == "Windows": command = self.general_config[0]['win'].replace("[file]", path + name + "." + self.format)
        elif platform.system() == "Darwin": command = self.general_config[0]['osx'].replace("[file]", path + name + "." + self.format)
        else: command = self.general_config[0]['linux'].replace("[file]", path + name + "." + self.format)
        subprocess.run(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)

