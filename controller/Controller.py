#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Dec 24 17:21:09 2020

@author: lorenzobacchiani
"""

import tkinter.filedialog as fd
import os
from threading import Thread
from utility.Log import Log
from controller.FileViewer import *
from utility.ObserverObjects import *
from utility.LexerErrorListener import LexerErrorListener
from utility.ParserErrorListener import ParserErrorListener
from antlr4 import *
from gen.SessionTypeLexer import SessionTypeLexer
from gen.SessionTypeParser import SessionTypeParser

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
        else: Log("Error Log", wscale=0.08, hscale=0.015, message=self.__string_cleaner(out))

    def gen_show_img(self, path, dotname, imgname, show=False):
        out = self.fv.generate(path, dotname, imgname)
        if not out == "": Log("Error Log", wscale=0.08, hscale=0.015, message=self.__string_cleaner(out))
        elif show: self.fv.show(path, imgname)

    def save_type_img(self, path, dotname, imgname, t):
        out = "Done"
        if not os.path.isfile(dotname + ".dot"): out = self.show_single_type(path, dotname, imgname, t, False)
        if out.__contains__("Done"): self.__save_img(path, imgname)

    def show_single_type(self, path, dotname, imgname, t, show=True):
        parsed_t = self.__check(t, None, "", "")[0]
        out = ""
        if not parsed_t == "":
            out = self.__call_outer_utility(parsed_t, "viewer", "Viewer")
            if out.__contains__("Done"): self.gen_show_img(path, dotname, imgname, show)
            else: Log("Error Log", wscale=0.08, hscale=0.015, message=self.__string_cleaner(out))
        return out

    ########################################################################

    def __single_execution(self, algconfig, t, s, options, pics, steps):
        parsed_t, parsed_s = self.__check(t, s, "(T)ype: ", "(S)upertype: ")
        if not parsed_t == "" and not parsed_s == "":
            out = self.__execute_command(algconfig, parsed_t, parsed_s, options, pics, steps)
            if os.path.isfile(algconfig['simulation_file'] + ".dot" if not platform.system() == "Windows" else algconfig['simulation_file'].replace("/","\\") + ".dot") and pics: AlgSuccessEvent(algconfig)
            Log("Subtyping Results", wscale=0.025, hscale=0.01, message=out)

    def __multiple_execution(self, t, s):
        outs = {}
        parsed_t, parsed_s = self.__check(t, s, "(T)ype: ", "(S)upertype: ")
        if not parsed_t == "" and not parsed_s == "":
            for algconfig in self.config: outs[algconfig['alg_name']] = self.__execute_command(algconfig, parsed_t, parsed_s, algconfig['standard_exec'], False, "")
            self.pretty_res(outs)

    def __execute_command(self, algconfig, t, s, options, pics, steps=""):
        command = algconfig['exec_comm'].replace("[flags]", algconfig["visual_flag"] + " " + options if pics else options).replace("[steps]", steps)
        t_name = self.__crete_tmp_type("tmp\\" if platform.system() == "Windows" else "tmp/", "t_temp.txt", t)
        s_name = self.__crete_tmp_type("tmp\\" if platform.system() == "Windows" else "tmp/", "s_temp.txt", s)
        command = command.replace("[t1]", t_name).replace("[t2]", s_name)
        if platform.system() == "Windows": command = algconfig['win'] + command + " && del " + t_name + " && del " + s_name
        else: command = algconfig['osx' if platform.system() == "Darwin" else 'linux'] + command + " && rm " + t_name + " && rm " + s_name
        out = str(subprocess.run(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT).stdout)
        return self.__string_cleaner(out)

    def pretty_res(self, outs):
        res = ""
        for key in outs: res += key + "\n" + outs[key] + "\n"
        Log("Subtyping Results", wscale=0.02, hscale=0.015, message=res)

    def __dualize(self, ty):
        dual = ""
        for c in ty:
            if c == '!': dual += '?'
            elif c == '?': dual += '!'
            elif c == '+': dual += '&'
            elif c == '&': dual += '+'
            else: dual += c
        return dual

    def __crete_tmp_type(self, directory, fn, t):
        if not os.path.exists(directory): os.makedirs(directory)
        f = open(directory + fn, "w")
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
        except: os.remove(path + imgname + "." + self.fv.format)

    def __parse(self, t):
        lexer = SessionTypeLexer(InputStream(t))
        lexer_error_listener = LexerErrorListener()
        lexer._listeners = [lexer_error_listener]
        stream = CommonTokenStream(lexer)
        parser = SessionTypeParser(stream)
        parser_error_listener = ParserErrorListener()
        parser._listeners = [parser_error_listener]
        tree = parser.start()
        return lexer_error_listener, parser_error_listener, tree.type

    def __check(self, t, s, t_location, s_location):
        t_lex_error, t_pars_error, parsed_t = self.__parse(t)
        t_error_message = ""
        s_error_message = ""
        if t_lex_error.error != 0: t_error_message += t_lex_error.message
        if t_pars_error.error != 0: t_error_message += t_pars_error.message
        if t_error_message == "": t_error_message = self.__semantic_check(parsed_t, t_location)
        else: t_error_message = t_location + "you had " + str(t_lex_error.error) + " lexical error(s) and " + str(t_pars_error.error) + " syntax error(s):\n" + t_error_message
        if s is not None:
            s_lex_error, s_pars_error, parsed_s = self.__parse(s)
            if s_lex_error.error != 0: s_error_message += s_lex_error.message
            if s_pars_error.error != 0: s_error_message += s_pars_error.message
            if s_error_message == "": s_error_message = self.__semantic_check(parsed_s, s_location)
            else: s_error_message = s_location + "you had " + str(s_lex_error.error) + " lexical error(s) and " + str(s_pars_error.error) + " syntax error(s):\n" + s_error_message
        if not t_error_message == "" or not s_error_message == "":
            Log(title="Error Log", wscale=0.08, hscale=0.015, message=(t_error_message if not t_error_message == "" else "") + ("\n\n" if not t_error_message  == "" and not s_error_message == "" else "") + (s_error_message if not s_error_message == "" else ""))
            return "",""
        return (parsed_t, parsed_s) if s is not None else (parsed_t, "")

    def __semantic_check(self, parsed_t, location):
        out = self.__call_outer_utility(parsed_t, "parser", "Main")
        return location + " " + self.__string_cleaner(out) + "\n" if len(self.__string_cleaner(out)) > 1 else ""


    def __call_outer_utility(self, parsed_t, mainfolder, executable):
        t_name = self.__crete_tmp_type("tmp\\" if platform.system() == "Windows" else "tmp/", "t_temp.txt", parsed_t)
        if platform.system() == "Windows":
            command = mainfolder + '\\win\\' + executable + " " + t_name + " && del " + t_name #ide
            #command = mainfolder + '\\' + executable + " " + t_name + " && del " + t_name   #standalone
        elif platform.system() == "Darwin":
            command = command = mainfolder + '/osx/' + executable + " " + t_name + " && rm " + t_name  #ide
            #command = mainfolder + "/" + executable + " " + t_name + " && rm " + t_name    #standalone
        else: command = mainfolder + "/linux/" + executable + " " + t_name + " && rm " + t_name  #linux distribution
        return str(subprocess.run(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT).stdout)

