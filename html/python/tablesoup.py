import BeautifulSoup as bs

file_text = '''
<html>
    <head>
        <title>The title</title>
    </head>
    <body>
        <table>
            <tr><td>TESTO1</td><td>TESTO1111</td></tr>
            <tr><td>TESTO2</td><td>TESTO2222</td></tr>
            <tr><td>TESTO3</td><td>TESTO3333</td></tr>
        </table>
    </body>
</html>
'''

soup = bs.BeautifulSoup(file_text)
stuff = [[cell.text for cell in row.findAll('td')] for row in soup.findAll('tr')]

print stuff