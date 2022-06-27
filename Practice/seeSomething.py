from sortAndOrganize import Process
from graphing import makeHisto


text = 'rawLATimes.txt'

table = Process().myHash(text)
table.__getitem__('the')
table.__getitem__('author')
makeHisto.makeIt(text)