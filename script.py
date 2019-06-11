# -*- coding: utf-8 -*-
"""
Created on Sat Jun  8 10:52:16 2019

@author: orlando
"""

#Importar una BD desde script

archivo = open (r'C:\Users\orlando\curso\Datos\BDVinicola.sql', 'r').read()

import sqlite3
sqlite3.complete_statement(archivo)

cone = sqlite3.connect(r'C:\Users\orlando\curso\Datos\BDVinos.db')
cone.executescript(archivo)
cone.commit()

print(list(cone.execute('select * from Vinos')))

cone.close()