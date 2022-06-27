from sortAndOrganize import Process
import matplotlib.pyplot as mpl

class makeHisto():
    def __init__(self):
        self.a = 'a'

    def distill(txt):
        table = Process().myAccountant(txt)
        nameList = []
        countList = []
        for i in table:
            for j in i:
                if j not in nameList:
                    nameList.append(j)
                    countList.append(i[j])
                else:
                    countList[nameList.index(j)] += i[j] 
        return countList

    def makeIt(txt):
        mpl.hist(makeHisto.distill(txt))
        mpl.show()
   
        





    