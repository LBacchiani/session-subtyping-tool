import tkinter as tk


class Log(tk.Toplevel):

    def __init__(self, title, wscale, hscale, message):
        tk.Toplevel.__init__(self, background='gray')
        self.width = self.winfo_screenwidth()
        self.height = self.winfo_screenheight()
        self.title(title)
        self.t = tk.Text(self, width=int(wscale * self.width), height=int(hscale*self.height))
        self.t.insert('1.0', message)
        self.scroll = tk.Scrollbar(self, orient=tk.VERTICAL, command=self.t.yview)
        self.scroll.pack(side='right', fill=tk.Y)
        self.t.config(state=tk.DISABLED, yscrollcommand=self.scroll.set)
        self.t['bg'] = 'gray'
        self.t.pack()
        self.button = tk.Button(self, text="OK")
        self.button['command'] = self.destroy
        self.button.pack(pady=10, padx=10, ipadx=20, side='right')
