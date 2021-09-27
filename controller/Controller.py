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
from utility.Location import *

class Controller:

    def __init__(self, config):
        self.fv = FileViewer()
        self.config = config

    def reload_config(self, config): self.config = config

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
        if os.path.isfile(dotname + "." + self.fv.format): self.fv.show(path, imgname)
        else: self.__img_op(path, dotname, imgname.replace(" ", "_"), self.__save_img)

    def gen_sim_img(self, path, dotname, imgname): self.__img_op(path, dotname, imgname, self.fv.show)

    def save_type_img(self, location, path, dotname, imgname, t): self.__single_type_op(location, path, dotname, imgname, t, self.__save_img)

    def show_single_type(self, location, path, dotname, imgname, t): self.__single_type_op(location, path, dotname, imgname, t, self.fv.show)

    ########################################################################

    def __single_type_op(self, location, path, dotname, imgname, t, op):
        if self.__create_dot_type_img(location, t): self.__img_op(path, dotname, imgname, op)

    def __img_op(self, path, dotname, imgname, op):
        out = self.fv.generate(path, dotname, imgname)
        if not out == "": Log("Error Log", wscale=0.08, hscale=0.015, message=self.__string_cleaner(out))
        else: op(path, imgname)

    def __create_dot_type_img(self, location, t):
        if location == Location.SUBTYPE:
            parsed, _ = self.__check(t, None)
            fname = "t_temp.txt"
        else:
            _, parsed = self.__check(None, t)
            fname = "s_temp.txt"
        if not parsed == "":
            out = self.__call_outer_utility(parsed, "viewer", "Viewer", fname)
            if not out.__contains__("Done"):
                Log("Error Log", wscale=0.08, hscale=0.015, message=self.__string_cleaner(out))
                return False
            return True
        else: return False

    def __single_execution(self, algconfig, t, s, options, pics, steps):
        parsed_t, parsed_s = self.__check(t, s)
        if not parsed_t == "" and not parsed_s == "":
            out = self.__execute_command(algconfig, options, pics, steps)
            if os.path.isfile(algconfig['simulation_file'] + ".dot" if not platform.system() == "Windows" else algconfig['simulation_file'].replace("/","\\") + ".dot") and pics: AlgSuccessEvent(algconfig)
            Log("Subtyping Results", wscale=0.06, hscale=0.01, message=out)

    def __multiple_execution(self, t, s):
        outs = {}
        parsed_t, parsed_s = self.__check(t, s)
        if not parsed_t == "" and not parsed_s == "":
            for algconfig in self.config: outs[algconfig['alg_name']] = self.__execute_command(algconfig, algconfig['standard_exec'], False, "")
            self.__pretty_res(outs)

    def __execute_command(self, algconfig, options, pics, steps=""):
        command = algconfig['exec_comm'].replace("[flags]", algconfig["visual_flag"] + " " + options if pics else options).replace("[steps]", steps)
        command = command.replace("[t1]", ("tmp\\" if platform.system() == "Windows" else "tmp/") + "t_temp.txt").replace("[t2]", ("tmp\\" if platform.system() == "Windows" else "tmp/") + "s_temp.txt")
        if platform.system() == "Windows": command = algconfig['win']
        else: command = algconfig['osx' if platform.system() == "Darwin" else 'linux'] + command
        out = str(subprocess.run(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT).stdout)
        return self.__string_cleaner(out)

    def __pretty_res(self, outs):
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

    def __check(self, t, s):
        t_parsed, t_error_message = self.__single_check(t, "(T)ype: ", "t_temp.txt") if t is not None else ("", "")
        s_parsed, s_error_message = self.__single_check(s, "(S)upertype: ", "s_temp.txt") if s is not None else ("", "")
        if not t_error_message == "" or not s_error_message == "":
            Log(title="Error Log", wscale=0.08, hscale=0.015, message=(t_error_message if not t_error_message == "" else "") + ("\n\n" if not t_error_message  == "" and not s_error_message == "" else "") + (s_error_message if not s_error_message == "" else ""))
            return "", ""
        if t is not None and s is not None: return t_parsed, s_parsed
        elif t is not None: return t_parsed, ""
        else: return "", s_parsed

    def __single_check(self, st, location, fname):
        error_msg = ""
        lex_error, pars_error, parsed = self.__parse(st)
        if lex_error.error != 0: error_msg += lex_error.message
        if pars_error.error != 0: error_msg += pars_error.message
        if error_msg == "": error_msg = self.__semantic_check(parsed, location, fname)
        else: error_msg = location + "you had " + str(lex_error.error) + " lexical error(s) and " + str(pars_error.error) + " syntax error(s):\n" + error_msg
        return parsed, error_msg

    def __semantic_check(self, parsed_t, location, fname):
        out = self.__call_outer_utility(parsed_t, "parser", "Main", fname)
        return location + self.__string_cleaner(out) + "\n" if len(self.__string_cleaner(out)) > 1 else ""

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

    def __call_outer_utility(self, parsed, mainfolder, executable, fname):
        t_name = self.__crete_tmp_type("tmp\\" if platform.system() == "Windows" else "tmp/", fname, parsed)
        if platform.system() == "Windows":
            command = mainfolder + '\\win\\' + executable + " " + t_name #ide
            #command = mainfolder + '\\' + executable + " " + t_name#standalone
        elif platform.system() == "Darwin":
            command = command = mainfolder + '/osx/' + executable + " " + t_name #ide
            #command = mainfolder + "/" + executable + " " + t_name #standalone
        else: command = mainfolder + "/linux/" + executable + " " + t_name #linux distribution
        return str(subprocess.run(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT).stdout)

