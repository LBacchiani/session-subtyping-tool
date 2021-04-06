import platform
from view.MainView import MainView
from controller.Controller import Controller
import os
import sys

def main():
    """
    #ONLY for STANDALONE VERSION
    application_path = os.path.dirname(sys.argv[0])
    os.chdir(application_path)
    os.chdir("../")
    if platform.system() == "Darwin": os.environ["PATH"] += os.pathsep + "/usr/local/bin"
    ################################
    """

    MainView().create_gui()


if __name__ == "__main__":
    main()
