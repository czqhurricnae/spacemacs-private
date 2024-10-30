import os

def getDirFiles(dir):
    fileList = []
    for ff in os.listdir(dir):
        # 过滤隐藏文件夹
        if ff.startswith('.'):
            continue
        filePath = os.path.join(dir, ff)
        if os.path.isdir(filePath):
            fileList.extend(getDirFiles(filePath))
        else:
            if ff.lower().endswith('.h') or ff.lower().endswith('.c') or ff.lower().endswith('.txt'):
                fileList.append(os.path.join(dir, ff))
    return fileList

originEncodeList = ['gbk', 'Big5', 'iso-latin-1-dos', 'gb2312']

if __name__ == '__main__':
    objFiles = getDirFiles('.')
    failList = []
    for f in objFiles:
        try:
            with open(f, 'rb')  as fpr:
                buf = fpr.read().decode('utf-8')
                # print('utf-8 encode  ok')
                #
                # 繁体转换成简体
                # from langconv import *
                # if f.find('main.c') != -1:
                #     simple = Converter('zh-hans').convert(buf)
                #     with open(f+'_simple.c', 'w', encoding='utf-8') as fpw:
                #         fpw.write(simple)
                #
                # 本身是UTF-8 格式 不需要转换
                continue
        except Exception as e:
            print(e)

        print(f)
        for ec in originEncodeList:
            try:
                with open(f, 'r', encoding=ec)  as fpr:
                    buf = fpr.read()
                    with open(f+'.utf8', 'w', encoding='utf-8') as fpw:
                        fpw.write(buf)
                os.remove(f)
                os.rename(f+'.utf8', f)
                break
            except:
                if f not in failList:
                    failList.append(f)
                # print('err f : %s encoding : %s'%(f, ec))
                # if os.path.exists(f+'.utf8'):
                #     os.remove(f+'.utf8')
    if failList:
        print('====================================')
        print('err files :')
        for f in failList:
            print(f)
        print('====================================')
    else:
        print('All Done!')
