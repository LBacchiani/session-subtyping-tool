import tkinter as tk
import platform
from utility.ObserverObjects import Observer
from tkinter import simpledialog

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
        self.t = None
        self.s = None

        ###Utilities###
        self.lastalg = ""
        self.subname = ""
        self.supname = ""
        self.config = config
        self.misc_options = {}
        self.exec_options = {}
        self.steps = 0
        self.pic = tk.BooleanVar()
        self.observe("SuccessEvent", self.__enable)

    def bind_type(self, session_type):
        if self.t is None: self.t = session_type
        else: self.s = session_type

    def configure_menu(self, controller):
        self.menu.add_cascade(label="File", menu=self.filemenu)
        self.menu.add_cascade(label="Algorithms", menu=self.algorithms)
        self.menu.add_cascade(label="Simulation Result", menu=self.simulation_res)
        self.menu.add_cascade(label="Settings", menu=self.settings)

        ###File Menu configuration####
        self.filemenu.add_command(label="Open (T)ype", command=lambda: controller.open_type("sub"))
        self.filemenu.add_command(label="Open (S)upertype", command=lambda: controller.open_type("sup"))
        self.filemenu.add_command(label="Save (T)ype", command=lambda: controller.save_type("sub", self.subname, self.t.get("1.0", "end-1c")))
        self.filemenu.add_command(label="Save (S)uperype", command=lambda: controller.save_type("sup", self.supname, self.s.get("1.0", "end-1c")))
        self.filemenu.add_command(label="Dualize Subtyping", command=lambda: self.__dualize(controller, self.t.get("1.0", "end-1c"), self.s.get("1.0", "end-1c")))
        ###Algorithms####
        for algconfig in self.config: self.algorithms.add_command(label=algconfig['alg_name'], command=lambda value=algconfig: self.__call_function(controller, value))

        ###Simulation result configuration###
        self.simulation_res.add_cascade(label="Save Image", command=lambda: self.__save(controller))
        self.simulation_res.add_cascade(label="Show", command=lambda: self.__show(controller))
        self.menu.entryconfig("Simulation Result", state="disabled")

        ###Pics format###
        self.pics_format.add_radiobutton(label=".png", command=lambda: self.__set_format("png", controller))
        self.pics_format.add_radiobutton(label=".pdf", command=lambda: self.__set_format("pdf", controller))
        self.pics_format.add_radiobutton(label=".jpeg", command=lambda: self.__set_format("jpeg", controller))
        self.pics_format.add_radiobutton(label=".svg", command=lambda: self.__set_format("svg", controller))

        if platform.system() == "Windows": self.pics_format.invoke(1)  # .png standard format
        else: self.pics_format.invoke(0)  # .png standard format

        ###Settings configuration###
        self.settings.add_cascade(label="Pics Format", menu=self.pics_format)
        self.settings.add_cascade(label="Algorithm Options", menu=self.options)
        for algconfig in self.config:
            options_menu = tk.Menu(self.options)
            if len(algconfig["execution_flag"]) > 0: self.__set_exec_flag(algconfig, options_menu)
            if len(algconfig['flag']) > 0: self.__set_misc_options(algconfig, options_menu)
            if algconfig['alg_name'] in self.misc_options.keys() or algconfig['alg_name'] in self.exec_options.keys():
                self.options.add_cascade(label=algconfig["alg_name"], menu=options_menu)
        self.settings.add_checkbutton(label="Generate Graphs", variable=self.pic)
        self.settings.add_command(label="Set Steps", command=self.__set_steps)

    def on_io_events(self, location, filename):
        if location == "sub": self.subname = filename
        else: self.supname = filename
        self.menu.entryconfig("Simulation Result", state="disabled")

    def on_dual_events(self, filename, location):
        if location == "sub": self.subname = filename
        else: self.supname = filename

    ##########################

    def __dualize(self, controller, t, s):
        controller.dualize(t, "sup")
        controller.dualize(s, "sub")


    def __set_steps(self):
        user_input = simpledialog.askstring("Algorithms step setting", "Please insert steps number")
        if user_input is not None and user_input.isnumeric(): self.steps = int(user_input)

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
                controller.gen_img(split[0] + ("\\" if platform.system() == "Windows" else "/"), split[1], split[1])
                controller.show_img(split[0] + ("\\" if platform.system() == "Windows" else "/"), split[1])
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
            if flag == algconfig['standard_exec']:
                bool.set(True)
            local[flag] = bool
            options_menu.add_checkbutton(label=flag, variable=bool, command=lambda value=flag: self.__unset_others(algconfig['alg_name'], value))
        self.exec_options[algconfig['alg_name']] = local

    def __unset_others(self, algname, flagname):
        for state in self.exec_options[algname]:
            if state != flagname: self.exec_options[algname][state].set(False)



