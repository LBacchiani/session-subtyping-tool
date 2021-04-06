import tkinter as tk

from controller.Controller import Controller
from utility.ObserverObjects import Observer
from view.WorkingArea import WorkingArea
from view.MenuView import MenuView
import subprocess
import platform
import json
import sys
import os
import tkinter.messagebox as mb
from utility.Location import *


class MainView(Observer):

    def __init__(self):
        Observer.__init__(self)
        self.window = tk.Tk()
        self.config = json.load(open("algorithms_config.json"))
        self.controller = Controller(self.config)
        self.window.protocol("WM_DELETE_WINDOW", self.on_closing)
        self.observe("ReloadConfigEvent", self.__on_reload_config)
        ###Utilities###
        self.maxwidth = self.window.winfo_screenwidth()
        self.maxheight = self.window.winfo_screenheight()
        self.width = int(self.maxwidth * 0.9)
        self.height = int(self.maxheight * 0.62)
        self.f1 = WorkingArea(self.width, self.height, Location.SUBTYPE, self.controller, self.window, self.config)
        self.f2 = WorkingArea(self.width, self.height, Location.SUPERTYPE, self.controller, self.window, self.config)
        self.custommenu = MenuView(self.window, self.config)




    def create_gui(self):
        ###Page configuration####
        self.window.title("Session Subtyping Tool")
        self.window.configure(background='gray')
        self.window.minsize(width=self.width, height=self.height)
        self.window.config(menu=self.custommenu.menu)
        self.custommenu.configure_menu(self.controller)
        self.f1.create_frame(tk.LEFT, self.custommenu)
        self.f2.create_frame(tk.RIGHT, self.custommenu)
        self.window.mainloop()

    def on_closing(self):
        if platform.system() == "Windows": subprocess.run("rmdir /Q /S tmp", shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        else: subprocess.run("rm -r tmp", shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        self.window.destroy()

    def __on_reload_config(self, newconf, alg_name):
        if alg_name == "": self.config.append(newconf)
        else:
            for i, conf in enumerate(self.config):
                if conf['alg_name'] == alg_name: self.config[i] = newconf
        with open('algorithms_config.json', 'w') as outfile: json.dump(self.config, outfile)
        if mb.askyesno("Restart", "Would you like to restart the program to make the modification effective?"):
            python = sys.executable
            os.execl(python, python, *sys.argv)



