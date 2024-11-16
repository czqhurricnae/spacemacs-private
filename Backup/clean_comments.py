import re
import os
import sys

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

def remove_comments(code):
    # 匹配单行注释
    code = re.sub(r'//.*', '', code)
    # 匹配多行注释
    code = re.sub(r'/\*.*?\*/', '', code, flags=re.DOTALL)
    return code


if __name__ == '__main__':
    if len(sys.argv) < 2:
        objFiles = getDirFiles('.')
        failList = []
        for f in objFiles:
            if f.endswith('.c') or f.endswith('.h'):
                # 读取文件内容
                with open(f, 'r') as file:
                    code = file.read()

                # 删除注释
                clean_code = remove_comments(code)

                # 写回文件
                with open(f.rsplit('.', 1)[0] + '_cleaned.' + f.rsplit('.', 1)[1], 'w') as file:
                    file.write(clean_code)

        print('All Done!')
    else:
        for f in sys.argv[1:]:
            if f.endswith('.c') or f.endswith('.h'):
                # 读取文件内容
                with open(f, 'r') as file:
                    code = file.read()

                # 删除注释
                clean_code = remove_comments(code)

                # 写回文件
                with open(f.rsplit('.', 1)[0] + '_cleaned.' + f.rsplit('.', 1)[1], 'w') as file:
                    file.write(clean_code)

        print('All Done!')
