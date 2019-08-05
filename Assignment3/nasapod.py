#!/usr/bin/env python3
import urllib.request
import json
import flask
from flask import request, jsonify, Flask, render_template, redirect


app = flask.Flask(__name__)
app.config["DEBUG"] = True

#Define APOD urllib
apodurl = 'https://api.nasa.gov/planetary/apod?'
mykey = 'api_key=DEMO_KEY'

#Call webservice
apodurlobj = urllib.request.urlopen(apodurl + mykey)

#read file like object
apodread = apodurlobj.read()
decodeapod = json.loads(apodread.decode('utf-8'))


@app.route('/')
def home():
    info = {
        'picurl': decodeapod['url'],
        'pictitle': decodeapod['title'],
        #'author': decodeapod['copyright'],
        'date': decodeapod['date'],
        'about': decodeapod['explanation'],
        'hidefurl': decodeapod['hdurl']
        }


    return render_template('index.html', info=info)
