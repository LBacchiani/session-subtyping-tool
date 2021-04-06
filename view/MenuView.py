import platform
import tkinter as tk

from utility.Log import Log
from utility.ObserverObjects import Observer
from tkinter import simpledialog
from utility.Location import *


from view.AlgorithmConfiguration import AlgorithmConfiguration


class MenuView(Observer):

    def __init__(self, window, config):
        Observer.__init__(self)
        self.menu = tk.Menu(window)
        self.filemenu = tk.Menu(self.menu)
        self.algorithms = tk.Menu(self.menu)
        self.settings = tk.Menu(self.menu)
        self.pics_format = tk.Menu(self.settings)
        self.simulation_res = tk.Menu(self.menu)
        self.fair_result = tk.Menu(self.simulation_res)
        self.unfair_result = tk.Menu(self.simulation_res)
        self.options = tk.Menu(self.settings)
        self.alg_conf_menu = tk.Menu(self.settings)
        self.edit_conf_menu = tk.Menu(self.alg_conf_menu)
        self.t = None
        self.s = None

        ###Utilities###
        self.lastalg = ""
        self.subname = ""
        self.supname = ""
        self.config = config
        self.misc_options = {}
        self.exec_options = {}
        self.maxwidth = window.winfo_screenwidth()
        self.maxheight = window.winfo_screenheight()
        self.steps = 0
        self.pic = tk.BooleanVar()
        self.observe("SuccessEvent", self.__enable)
        self.observe("IOEvent" + Location.SUBTYPE.value, self.__on_io_events_sub)
        self.observe("IOEvent" + Location.SUPERTYPE.value, self.__on_io_events_sup)
        self.observe("DualEvent" + Location.SUBTYPE.value, self.__on_dual_events_sub)
        self.observe("DualEvent" + Location.SUPERTYPE.value, self.__on_dual_events_sup)

    def bind_type(self, session_type):
        if self.t is None: self.t = session_type
        else: self.s = session_type

    def configure_menu(self, controller):
        self.menu.add_cascade(label="File", menu=self.filemenu)
        self.menu.add_cascade(label="Algorithms", menu=self.algorithms)
        self.menu.add_cascade(label="Simulation Result", menu=self.simulation_res)
        self.menu.add_cascade(label="Settings", menu=self.settings)

        ###File Menu configuration####
        self.filemenu.add_command(label="Open (T)ype", command=lambda: controller.open_type(Location.SUBTYPE.value))
        self.filemenu.add_command(label="Open (S)upertype", command=lambda: controller.open_type(Location.SUPERTYPE.value))
        self.filemenu.add_command(label="Save (T)ype", command=lambda: controller.save_type(Location.SUBTYPE.value, self.subname, self.t.get("1.0", "end-1c")))
        self.filemenu.add_command(label="Save (S)uperype", command=lambda: controller.save_type(Location.SUPERTYPE.value, self.supname, self.s.get("1.0", "end-1c")))
        self.filemenu.add_command(label="Dual Subtyping", command=lambda: self.__dualize(controller, self.t.get("1.0", "end-1c"), self.s.get("1.0", "end-1c")))

        ###Algorithms####
        for algconfig in self.config:
            self.algorithms.add_command(label=algconfig['alg_name'], command=lambda value=algconfig: self.__call_function(controller, value))
            self.edit_conf_menu.add_command(label=algconfig['alg_name'], command=lambda value=algconfig: AlgorithmConfiguration(self.maxwidth, self.maxheight,value))
        self.algorithms.add_command(label="Run All", command=lambda: controller.run_all(self.t.get("1.0", "end-1c"), self.s.get("1.0", "end-1c")))

        ###Simulation result configuration###
        self.simulation_res.add_command(label="Save Image", command=lambda: self.__save(controller))
        self.simulation_res.add_command(label="Show", command=lambda: self.__show(controller))
        self.menu.entryconfig("Simulation Result", state="disabled")

        ###Pics format###
        for format in ["png", "pdf", "jpeg", "svg"]: self.pics_format.add_radiobutton(label="."+format, command=lambda f=format: self.__set_format(f, controller))

        if platform.system() == "Darwin": self.pics_format.invoke(0)  # .png standard format
        else: self.pics_format.invoke(1)  # .png standard format


        ###Settings configuration###
        self.settings.add_cascade(label="Pics Format", menu=self.pics_format)
        self.settings.add_cascade(label="Algorithm Options", menu=self.options)
        for algconfig in self.config:
            options_menu = tk.Menu(self.options)
            if "[steps]" in algconfig['exec_comm']: options_menu.add_command(label="Set Steps", command=self.__set_steps)
            if len(algconfig["execution_flag"]) > 0: self.__set_exec_flag(algconfig, options_menu)
            if len(algconfig['flag']) > 0: self.__set_misc_options(algconfig, options_menu)
            if algconfig['alg_name'] in self.misc_options.keys() or \
                    algconfig['alg_name'] in self.exec_options.keys() or "[steps]" in algconfig['exec_comm']:
                self.options.add_cascade(label=algconfig["alg_name"], menu=options_menu)
        self.settings.add_checkbutton(label="Generate Graphs", variable=self.pic)
        self.settings.add_cascade(label="Algorithm Configurations", menu=self.alg_conf_menu)
        self.alg_conf_menu.add_command(label="Add algorithm", command=lambda: AlgorithmConfiguration(self.maxwidth, self.maxheight))
        self.alg_conf_menu.add_cascade(label="Edit configuration", menu=self.edit_conf_menu)
        self.pic.set(True)

    def __on_io_events_sub(self, filename):
        self.subname = filename
        self.menu.entryconfig("Simulation Result", state="disabled")

    def __on_io_events_sup(self, filename):
        self.supname = filename
        self.menu.entryconfig("Simulation Result", state="disabled")

    def __on_dual_events_sub(self, filename): self.subname = filename

    def __on_dual_events_sup(self, filename): self.supname = filename

    def __dualize(self, controller, t, s):
        controller.dualize(t, Location.SUPERTYPE.value)
        controller.dualize(s, Location.SUBTYPE.value)

    def __set_steps(self):
        user_input = simpledialog.askstring("Algorithms step setting", "Insert steps number of the next simulation")
        if user_input is not None and user_input.isnumeric():
            self.steps = int(user_input)
            if self.steps <= 0: Log("Warning", wscale=0.025, hscale=0.01, message="Please insert a number > 0")

    def __save(self, controller):
        for algconfig in self.config:
            if self.lastalg == algconfig['alg_name']:
                split = algconfig['simulation_file'].split("/")
                controller.save_simulation_img(split[0] + ("\\" if platform.system() == "Windows" else "/"), split[1], self.lastalg + "_" + self.subname + "_" + self.supname)
                return

    def __show(self, controller):
        for algconfig in self.config:
            if algconfig['alg_name'] == self.lastalg:
                split = algconfig['simulation_file'].split("/")
                controller.gen_sim_img(split[0] + ("\\" if platform.system() == "Windows" else "/"), split[1], split[1])
                return

    def __enable(self, algconfig):
        self.lastalg = algconfig['alg_name']
        self.menu.entryconfig("Simulation Result", state="normal")

    def __set_format(self, extension, controller): controller.fv.format = extension

    def __call_function(self, controller, algconfig):
        options = ""
        if algconfig['alg_name'] in self.exec_options.keys(): options += self.__set_options(self.exec_options, algconfig['alg_name'])
        if algconfig['alg_name'] in self.misc_options.keys(): options += self.__set_options(self.misc_options, algconfig['alg_name'])
        controller.call_algorithm(algconfig, self.t.get("1.0", "end-1c"), self.s.get("1.0", "end-1c"), options, self.pic.get(), "" if self.steps == 0 else str(self.steps))
        self.steps = 0

    def __set_options(self, iterable, algname):
        options = ""
        for k in iterable[algname].keys():
            if iterable[algname][k].get(): options += " " + k
        return options

    def __set_misc_options(self, algconfig, options_menu):
        local = {}
        self.misc_options[algconfig['alg_name']] = {}
        for flag in algconfig['flag'].split(','):
            bool = tk.BooleanVar()
            local[flag] = bool
            options_menu.add_checkbutton(label=flag, variable=bool)
        self.misc_options[algconfig['alg_name']] = local

    def __set_exec_flag(self, algconfig, options_menu):
        local = {}
        self.exec_options[algconfig['alg_name']] = {}
        for flag in algconfig['execution_flag'].split(','):
            bool = tk.BooleanVar()
            if flag == algconfig['standard_exec']: bool.set(True)
            local[flag] = bool
            options_menu.add_checkbutton(label=flag, variable=bool, command=lambda value=flag: self.__unset_others(algconfig['alg_name'], value))
        self.exec_options[algconfig['alg_name']] = local

    def __unset_others(self, algname, flagname):
        for state in self.exec_options[algname]:
            if state != flagname: self.exec_options[algname][state].set(False)




