import platform
from view.MainView import MainView
from controller.Controller import Controller
import os
import sys
import json
import subprocess
from tkinter.messagebox import *

def main():
    """
    #ONLY for STANDALONE VERSION
    application_path = os.path.dirname(sys.argv[0])
    os.chdir(application_path)
    os.chdir("../")
    ####Only for OSx standalone###
    if platform.system() == "Darwin": os.environ["PATH"] += os.pathsep + "/usr/local/bin"
    ################################
    """
    f = open("config.json")
    config = json.load(f)
    controller = Controller(config)
    MainView(controller, config).create_gui()


if __name__ == "__main__":
    main()
