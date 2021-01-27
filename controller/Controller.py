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
import os

class Controller:

    def __init__(self, config):
        self.fv = FileViewer(config)
        self.config = config

    def call_algorithm(self, algconfig, t, s, options, pics, steps): Thread(target=lambda: self.__single_execution(algconfig, t, s, options, pics, steps)).start()

    def run_all(self, t, s): Thread(target=lambda: self.__multiple_execution(t, s)).start()

    def dualize(self, t, location): DualEvent(location, self.__dualize(t))

    def open_type(self, location):
        try:
            f = open(fd.askopenfilename(), "r")
            name = f.name.split("/")[-1]
            IOEvent(location, f.read(), name)
        except: pass

    def save_type(self, location, currentname, text):
        try:
            f = fd.asksaveasfile(mode='w', defaultextension=".txt", initialfile=currentname if not currentname == "" else "new_type")
            name = f.name.split("/")[-1]
            f.write(text)
            f.close()
            IOEvent(location, text, name)
        except: pass

    def save_simulation_img(self, path, dotname, imgname):
        out = ""
        if not os.path.isfile(dotname + "." + self.fv.format): out = self.fv.generate(path, dotname, imgname.replace(" ", "_"))
        if out == "": self.__save_img(path, imgname.replace(" ", "_"))
        else: showerror("Syntax Error", message=self.__string_cleaner(out))

    def gen_img(self, path, dotname, imgname):
        out = self.fv.generate(path, dotname, imgname)
        if not out == "": showerror("Syntax Error", message=self.__string_cleaner(out))

    def show_img(self, path, imgname): self.fv.show(path, imgname)

    def save_type_img(self, path, dotname, imgname, t):
        out = "Done"
        if not os.path.isfile(dotname + ".dot"): out = self.show_single_type(path, dotname, imgname, t, False)
        if out.__contains__("Done"): self.__save_img(path, imgname)

    def show_single_type(self, path, dotname, imgname, t, show=True):
        t_name = self.__crete_tmp_type("tmp\\" if platform.system() == "Windows" else "tmp/", "t_temp.txt", t)
        if platform.system() == "Windows":
            command = 'viewer\\win\\Viewer ' + t_name + " && del " + t_name #ide
            #command = 'viewer\\viewer ' + t_name + " && del " + t_name   #standalone
        elif platform.system() == "Darwin":
            command = "viewer/osx/Viewer " + t_name + " && rm " + t_name  #ide
            #command = 'viewer/viewer ' + t_name + " && rm " + t_name    #standalone
        else:
            command = "viewer/linux/Viewer " + t_name + " && rm " + t_name  #linux distribution

        out = str(subprocess.run(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT).stdout)
        if out.__contains__("Done"):
            self.gen_img(path, dotname, imgname)
            if show: self.show_img(path, imgname)
        else: showinfo("Syntax Error", message=self.__string_cleaner(out))
        return out

    ########################################################################

    def __execute_command(self, algconfig, t, s, options, pics, steps=""):
        command = algconfig['exec_comm'].replace("[flags]", algconfig["visual_flag"] + " " + options if pics else options).replace("[steps]", steps)
        t_name = self.__crete_tmp_type( "tmp\\" if platform.system() == "Windows" else "tmp/", "t_temp.txt", t)
        s_name = self.__crete_tmp_type( "tmp\\" if platform.system() == "Windows" else "tmp/", "s_temp.txt", s)
        command = command.replace("[t1]", t_name).replace("[t2]", s_name)
        if platform.system() == "Windows": command = algconfig['win'] + command + " && del " + t_name + " && del " + s_name
        elif platform.system() == "Darwin": command = algconfig['osx'] + command + " && rm " + t_name + " && rm " + s_name
        else: command = algconfig['linux'] + command + " && rm " + t_name + " && rm " + s_name
        out = str(subprocess.run(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT).stdout)
        return self.__string_cleaner(out)

    def __single_execution(self, algconfig, t, s, options, pics, steps):
        out = self.__execute_command(algconfig, t, s, options, pics, steps)
        if out.__contains__("True") and pics: AlgSuccessEvent(algconfig)
        showinfo("Subtyping Result", message=out)

    def __multiple_execution(self, t, s):
        outs = {}
        for algconfig in self.config:
            out = self.__execute_command(algconfig, t, s, algconfig['standard_exec'], False, "")
            outs[algconfig['alg_name']] = out
        self.pretty_res(outs)

    def pretty_res(self, outs):
        res = ""
        for key in outs:
            res += key + "\n" + outs[key] + "\n"
        showinfo("Subtyping Result", message=res)

    def __dualize(self, ty):
        dual = ""
        for c in ty:
            if c == '!': dual += '?'
            elif c == '?': dual += '!'
            elif c == '+': dual += '&'
            elif c == '&': dual += '+'
            else: dual += c
        return dual

    def __crete_tmp_type(self, dir, fn, t):
        if not os.path.exists(dir): os.makedirs(dir)
        f = open(dir + fn, "w")
        f.write(t)
        f.close()
        return f.name

    def __string_cleaner(self, s): return s.replace("b'", "").replace("'", "").replace('b"', '').replace('"', '').replace("\\n", "\n").replace("\\r", "")

    def __save_img(self, path, imgname):
        try:
            f = fd.asksaveasfile(mode='w', initialfile=imgname, defaultextension=self.fv.format)
            if platform.system() == "Windows": command = "copy " + path + imgname + "." + self.fv.format + " " + f.name.replace("/", "\\") + " && del " + path + imgname + "." + self.fv.format
            else: command = "mv " + path + imgname + "." + self.fv.format + " " + f.name
            subprocess.run(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        except: pass