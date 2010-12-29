import tkinter as tk

class Window(tk.Frame):
    def __init__(self, root):
        tk.Frame.__init__(self, root)
        value = 0
        def inc():
            nonlocal value
            value += 1
            self.label.config(text=str(value))
        self.label = tk.Label(text=str(value))
        self.label.pack()
        self.button = tk.Button(text="Increment", command=inc)
        self.button.pack()

root = tk.Tk()
w = Window(root)
w.pack()
tk.mainloop()