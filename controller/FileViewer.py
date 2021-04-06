import subprocess
import platform
import json
from utility.ObserverObjects import Observer


class FileViewer(Observer):

    def __init__(self):
        Observer.__init__(self)

        self.format = ""
        self.general_config = json.load(open("general_config.json"))
        self.observe("FormatEvent", self.__change_format_event)

    def generate(self, path, dotname, imgname):
        command = "dot -T" + self.format + " " + path + dotname + ".dot > " + path + imgname + "." + self.format
        out = str(subprocess.run(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT).stdout)
        return out if len(out) > 3 else ""

    def show(self,  path, name):
        if platform.system() == "Windows": command = self.general_config[0]['win']
        elif platform.system() == "Darwin": command = self.general_config[0]['osx']
        else: command = self.general_config[0]['linux']
        command = command.replace("[file]", path + name + "." + self.format)
        subprocess.run(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)

    def __change_format_event(self, new_format): self.format = new_format

