import pythoncom
import win32com.client   
shell = win32com.client.Dispatch("WScript.Shell")  

shell.SendKeys("ciaooooo")
