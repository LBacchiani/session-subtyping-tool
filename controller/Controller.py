#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Dec 24 17:21:09 2020

@author: lorenzobacchiani
"""

import tkinter.filedialog as fd
from threading import Thread
from tkinter.messagebox import *
from controller.FileViewer import *
from utility.ObserverObjects import *
from os import path


class Controller:

    def __init__(self, config):
        self.fv = FileViewer(config)
        self.config = config

    def call_algorithm(self, algconfig, t, s, options, pics, steps): Thread(target=lambda: self.__alg_target(algconfig, t, s, options, pics, steps)).start()

    def dual(self, t, s):
        if t == "" or s == "":
            showerror("Error", message="Missing type(s)")
        else:
            DualEvent("sub", self.__dualize(s))
            DualEvent("sup", self.__dualize(t))

    def open_type(self, location):
        try:
            f = open(fd.askopenfilename(), "r")
            name = f.name.split("/")[-1]
            IOEvent(location, f.read(), name)
        except: pass

    def save_type(self, location, text):
        try:
            f = fd.asksaveasfile(mode='w', defaultextension=".txt", initialfile=location)
            name = f.name.split("/")[-1]
            f.write(text)
            f.close()
            IOEvent(location, text, name)
        except: pass

    def save_simulation_img(self, name, customname):
        if not path.isfile(name + "." + self.fv.format): self.gen_img(name)
        try:
            f = fd.asksaveasfile(mode='w', initialfile=customname.replace(" ", "_") + "." + self.fv.format)
            if platform.system() == "Windows": command = "copy " + name + "." + self.fv.format + " " + f.name.replace("/", "\\") + " && del " + name + "." + self.fv.format
            else: command = "mv " + name + "." + self.fv.format + " " + f.name
            subprocess.run(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        except: pass

    def save_type_img(self, name, t, customname):
        out = "Done"
        if not path.isfile(name + ".dot"): out = self.show_single_type(name, t, False)
        if out.__contains__("Done"): self.save_simulation_img(name, customname)

    def gen_img(self, name): self.fv.generate(name)

    def show_img(self, name): self.fv.show(name)

    def show_single_type(self, typename, t, show=True):
        if platform.system() == "Windows":
            command = 'viewer\\win\\viewer "' + t + '"' #ide
            #command = 'viewer\\viewer "' + t + '"' #standalone
            out = str(subprocess.run(command, shell=True, stdout=subprocess.PIPE,stderr=subprocess.STDOUT).stdout) #for IDE
        else:
            command = 'viewer/osx/viewer "' + t + '"' #ide
            #command = 'viewer/viewer "' + t + '"' #standalone
            out = str(subprocess.run(command, shell=True, stdout=subprocess.PIPE,stderr=subprocess.STDOUT).stdout) #for IDE
        if out.__contains__("Done"):
            self.gen_img(typename)
            if show: self.show_img(typename)
        else: showinfo("Syntax Error", message=self.string_cleaner(out))
        return out

    ########################################################################

    def __execute_command(self, algconfig, t, s, options, pics, steps=""):
        command = algconfig['exec_comm'].replace("[flags]", algconfig["visual_flag"] + " " + options if pics else options).replace("[steps]", steps)
        if platform.system() == "Windows": out = str(subprocess.run(algconfig['win'] + command.replace("[t1]", '"' + t + '"').replace("[t2]", '"' + s + '"'), shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT).stdout)
        else: out = str(subprocess.run(algconfig['osx'] + command.replace("[t1]", "'" + t + "'").replace("[t2]", "'" + s + "'"), shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT).stdout)
        return self.string_cleaner(out)

    def __alg_target(self, algconfig, t, s, options, pics, steps):
        out = self.__execute_command(algconfig, t, s, options, pics, steps).replace("b'", "").replace("'", "").replace('b"','').replace('"','').replace("\\n","\n")
        if out.__contains__("True") and pics: AlgSuccessEvent(algconfig)
        showinfo("Subtyping Result", message=out)

    def __dualize(self, ty):
        dual = ""
        for c in ty:
            if c == '!': dual += '?'
            elif c == '?': dual += '!'
            else: dual += c
        return dual

    def string_cleaner(self, s): return s.replace("b'", "").replace("'", "").replace('b"','').replace('"','').replace("\\n","\n").replace("\\r", "")