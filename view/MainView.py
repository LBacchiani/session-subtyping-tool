import tkinter as tk
from view.WorkingArea import WorkingArea
from view.MenuView import MenuView




class MainView:

    def __init__(self, controller, config):
        self.window = tk.Tk()
        self.config = config

        ###Utilities###
        self.maxwidth = self.window.winfo_screenwidth()
        self.maxheight = self.window.winfo_screenheight()
        self.width = int(self.maxwidth * 0.9)
        self.height = int(self.maxheight * 0.62)
        self.f1 = WorkingArea(self.width, self.height, "sub", controller, self.window, config)
        self.f2 = WorkingArea(self.width, self.height, "sup", controller, self.window, config)
        self.custommenu = MenuView(self.window, config)
        self.controller = controller


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
