import Tkinter as tk

class NumberModel(object):
    def __init__(self, val=0):
        self.value = val

    def increment(self):
        self.value += 1

    def decrement(self):
        self.value -= 1

class Controller(object):
    def __init__(self, model):
        self.model = model

    def __getattr__(self, attr_name):
        return getattr(self.model, attr_name)

class Window(tk.Frame):
    def __init__(self, model, controller, *args):
        tk.Frame.__init__(self, *args)
        self.entry = tk.Label(self)
        self.quit_button = tk.Button(self, text='Quit', command=root.destroy)
        self.inc_button = tk.Button(self, text='+', command=self.increment)
        self.dec_button = tk.Button(self, text='-', command=self.decrement)
        self.entry.grid(row=0, columnspan=2)
        self.inc_button.grid(row=1, column=0)
        self.dec_button.grid(row=1, column=1)
        self.quit_button.grid(row=2, columnspan=2)
        self.pack()

        self.model = model
        self.controller = controller

        self.update()

    def update(self):
        self.entry.configure(text=str(self.model.value))

    def increment(self):
        self.controller.increment()
        self.update()

    def decrement(self):
        self.controller.decrement()
        self.update()


if __name__ == '__main__':
    root = tk.Tk()
    model = NumberModel()
    controller = Controller(model)
    w = Window(model, controller, root)

    root.mainloop()
