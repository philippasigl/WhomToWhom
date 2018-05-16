from flask import Flask, render_template, request, redirect, url_for, flash

#flask and file configuration
app = Flask(__name__)
app.secret_key = 'some_secret'

#in file reading configuration is found in graph_data.py
@app.route('/', methods = ['GET','POST'])
def upload_file():
    return render_template('index.html')

if __name__ == '__main__': 
   app.run(debug = True)