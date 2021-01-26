import tkinter as tk
from utility.ObserverObjects import Observer
import platform

class WorkingArea(Observer):

    def __init__(self, width, height, location, controller, root, config):
        Observer.__init__(self)

        ###Utilities###
        self.lastty = ""
        self.laststy = ""
        self.filename = ""
        self.width = width
        self.height = height
        self.rpoint = 0
        self.cpoint = 0
        self.varpos = tk.StringVar()
        self.varpos.set("1:1")
        self.config = config
        self.location = location

        ###GUI Elements###
        self.window = tk.Frame(root, width=self.width/2, height=self.height, background='gray')
        if location == "sub": self.ty = tk.Label(self.window, text="(T)ype: " + self.filename)
        else: self.ty = tk.Label(self.window, text="(S)upertype: " + self.filename)
        self.t = tk.Text(self.window, font=("Purisa",15), cursor="ibeam")
        self.show = tk.Button(self.window, text="Show Image", background='gray', highlightcolor='gray')
        self.save = tk.Button(self.window, text="Save Image", background='gray', highlightcolor='gray')
        self.dual = tk.Button(self.window, text="Dual Type", background='gray', highlightcolor='gray')
        self.d2 = tk.Label(self.window, background='lightgray')
        self.d3 = tk.Label(self.window, background='lightgray')
        self.d4 = tk.Label(self.window, background='lightgray')
        self.scroll = tk.Scrollbar(self.window, orient=tk.VERTICAL)
        self.pos = tk.Label(self.window, textvariable=self.varpos, background='gray',font=("Purisa",15))
        self.custommenu = None
        self.controller = controller
        self.observe("IOEvent" + location, self.__on_io_events)
        self.observe("DualEvent" + location, self.__on_dual_events)


    def create_frame(self, s, menu):
        self.window.pack(side=s)
        self.custommenu = menu
        self.custommenu.bind_type(self.t)
        self.d3.place(x=int(self.width * 0.493), y=0, width=.001, height=self.height)
        self.d4.place(x=int(self.width * 0.528), y=0, width=.001, height=self.height)

        ###Labels####
        self.ty.configure(background='gray')
        self.ty.place(x=int(self.width * .19), y=5)
        self.pos.place(x=int(self.width/2 * .89), y=int(self.height * .94))

        ###Text areas####
        self.t.config(yscrollcommand=self.scroll.set)
        self.t.place(x=int(self.width * .025), y=30, width=int(self.width * .46), height=int(self.height * .8))
        self.t.bind("<KeyRelease>", self.__on_button_press)
        self.t.bind("<ButtonRelease-1>", lambda event: self.__update_pos())

        ###Buttons###
        self.show.place(x=int(self.width * .07), y=int(self.height * .9))
        self.dual.place(x=int(self.width * .21), y=int(self.height * .9))
        self.save.place(x=int(self.width * .35), y=int(self.height * .9))
        self.show.configure(command=lambda: self.controller.show_single_type(("tmp\\" if platform.system() == "Windows" else "tmp/"), "type_cfsm", self.location, self.t.get("1.0", "end-1c")))
        self.save.configure(command=lambda: self.controller.save_type_img(("tmp\\" if platform.system() == "Windows" else "tmp/"), "type_cfsm", (self.filename[:-4] if not self.filename == "" else self.location), self.t.get("1.0", "end-1c")))
        self.dual.configure(command=lambda: self.controller.dualize(self.t.get("1.0", "end-1c"), self.location))

        ###Scroll bar###
        self.scroll.config(command=self.t.yview)
        self.scroll.pack(side=tk.RIGHT, fill=tk.Y)
        self.scroll.place(x=int(self.width * 0.48), y=30, height=int(self.height * 0.8))

    def __on_dual_events(self, text):
        self.filename = "dual_" + self.filename if not self.filename == "" else "dual.txt"
        self.ty.configure(text=self.ty['text'].split(" ")[0] + " " + self.filename + "*")
        self.t.delete('1.0', tk.END)
        self.t.insert('1.0', text)
        self.lastty = text
        self.custommenu.on_dual_events(self.filename[:-4], self.location)

    def __on_io_events(self, text, filename):
        self.ty.configure(text=self.ty['text'].split(" ")[0] + " " + filename)
        self.t.delete('1.0', tk.END)
        self.t.insert('1.0', text)
        self.lastty = text
        self.filename = filename
        self.custommenu.on_io_events(self.location, filename[:-4])
        self.__update_pos()

    def __on_button_press(self, event):
        self.__on_text_press()
        self.pos.after(50, self.__update_pos)

    def __on_text_press(self):
        if self.t.get("1.0", "end-1c") == self.lastty: self.ty.configure(text=self.ty['text'].replace("*", ""))
        elif "*" not in self.ty['text']: self.ty.configure(text=self.ty['text'] + "*")

    def __update_pos(self):
        coords = self.t.index(tk.INSERT).split(".")
        self.cpoint, self.rpoint = int(coords[0]), int(coords[1]) + 1
        self.varpos.set(str(self.cpoint) + ":" + str(self.rpoint))
