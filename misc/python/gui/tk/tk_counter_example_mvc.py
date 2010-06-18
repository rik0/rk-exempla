import bisect

import Tkinter as tk

class NumberModel(object):
    def __init__(self, val=0):
        self.value = val

    def increment(self):
        self.value += 1

    def decrement(self):
        self.value -= 1

class GraphModel(object):
    def __init__(self):
        self.points = []

    def add_point(self, point):
        bisect.insort(self.points, point)

    def __len__(self):
        return len(self.points)


class Controller(object):
    def __init__(self, number_model, graph_model):
        self.number_model = number_model
        self.graph_model = graph_model

    def increment(self):
        self.number_model.increment()
        self._update_graph()

    def decrement(self):
        self.number_model.decrement()
        self._update_graph()

    def _update_graph(self):
        x_value = len(self.graph_model)
        y_value = self.number_model.value
        self.graph_model.add_point((x_value, y_value))

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
        self.quit_button.grid(row=2)
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
    number_model = NumberModel()
    graph_model = GraphModel()
    controller = Controller(number_model, graph_model)
    w = Window(number_model, controller, root)

    root.mainloop()
