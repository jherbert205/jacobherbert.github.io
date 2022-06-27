from nltk.stem import WordNetLemmatizer
from pathlib import Path

filePath = 'rawLATimes.txt'

class Node:
    def __init__(self, data=None, next=None):
        self.data = data
        self.next = next

class LinkedList:
    def __init__(self):
        self.head = None
    
    def insertBegin(self, data):
        node = Node(data, self.head)
        self.head = node
    
    def insertEnd(self, data):
        if self.head is None:
            self.head = Node(data, None)
            return
        itr = self.head
        while itr.next:
            itr = itr.next
        itr.next = Node(data, None)

    def print(self):
        if self.head is None:
            print("Word is not in articles")
            return
        itr = self.head
        llstr = ''
        while itr:
            llstr += str(itr.data)+' -> ' if itr.next else str(itr.data)
            itr = itr.next
        print(llstr)


class outputHash:
    def __init__(self):
        self.size = 10000
        self.tab = [[] for i in range(self.size)]
    
    def collision(self, key, att):
        h = self.makeHash(key)
        h = (h + att^2) % self.size
        return h


    def makeHash(self, key):
        h = 1
        x = 1
        y = 3
        for char in key:
            h += (y*ord(char)^2)+(ord(char)^x) 
            x += 4
            y += 2
        b = h % self.size
        return b


    def __setitem__(self, key, value):
        h = self.makeHash(key)  
        lnk = LinkedList()
        if self.tab[h] == []:
            lnk.insertBegin(value)
            self.tab[h] = (key, lnk)
        elif self.tab[h][0] == key:
            self.tab[h][1].insertEnd(value)
        else:
            att = 1
            x = False
            while x == False:
                h = self.collision(key, att)
                if self.tab[h] == []:
                    lnk.insertBegin(value)
                    self.tab[h] = (key, lnk)
                    x = True
                elif self.tab[h][0] == key:
                    self.tab[h][1].insertEnd(value)
                    x = True
                else:
                    att += 1

            



    def __getitem__(self, key):
        h = self.makeHash(key)
        if self.tab[h] and self.tab[h][0]==key:
            return print(self.tab[h][0]), self.tab[h][1].print()
        elif self.tab[h]:
            att = 1
            while False and att < 10:
                h = self.collision(key, att)
                if self.tab[h] != [] and self.tab[h][0] == key:
                    return print(self.tab[h][0]), self.tab[h][1].print()
                    x = True
                else:
                    att += 1
                    if att == 9:
                        return print("This word is not in the articles")
        else:
            return print("This word is not in the articles")
    
    def cleaner(self):
        for i in self.tab:
            i == []


class Process:

    def impAndClean(self, filePath):

        myText = Path(filePath).read_text()

        myText = myText.replace("<p>", " ")
        myText = myText.replace("</p>", " ")
        myText = myText.lower()
        strippers = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0", ",", "'", "!", "?",  ".", "(", ")", '"', ":", ";"]
        for i in strippers:
            myText = myText.replace(i, "")
        myText = myText.replace("-", " ")
        myText = myText.split()

        return myText


    def mySplitter(self,filePath):
        txt = self.impAndClean(filePath)
        killSwitch = 0
        articleNum = -1
        theCount = 0
        articles = []
        lemon = WordNetLemmatizer()
        while theCount < len(txt):
            if killSwitch == 0:
                if txt[theCount] == "<text>":
                    killSwitch = 1
                    articles.append([])
                    articleNum += 1
            else:
                if txt[theCount] == "</text>":
                    killSwitch = 0
                    
                else:
                    a = lemon.lemmatize(txt[theCount])
                    articles[articleNum].append(a)
                    
            theCount+=1            
        return articles

    
    def myAccountant(self, filePath):
        boof = []
        witch = -1
        art = 0
        for i in self.mySplitter(filePath):
            boof.append({})
            witch += 1
            art+=1
            for word in i:
                if word in boof[witch]:
                    boof[witch][word] += 1
                else:
                    boof[witch][word] = 1
        return boof 



    def myHash(self, filePath):
        hashy = outputHash()
        art = 0
        for i in self.myAccountant(filePath):
            art+=1
            for word in i.keys():
                hashy.__setitem__(word, [art, i[word]])
                #hashy.__getitem__(word)
        return hashy
    
    
    











