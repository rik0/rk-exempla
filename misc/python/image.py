from Tkinter import *
from PIL import Image, ImageTk
root = Tk()
root.title('titolo')
raw_input("pause")
bmp = Image.open("icona.bmp")
bmp = ImageTk.PhotoImage(bmp)
print bmp.width(),bmp.height()
cv = Canvas(root,width=100,height=100,bg="white")
cv.pack()
cv.create_image(0, 0, image=bmp, anchor='nw')
cv.create_text(15, 15, text="testo", fill="blue", anchor='nw')
root.mainloop()
