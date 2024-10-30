import re
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

def convert_comments(code):
    # 匹配双反斜杠风格的注释
    pattern = re.compile(r'(\s*)(.*?)//(.*)')
    lines = code.split('\n')
    new_lines = []

    for line in lines:
        match = pattern.match(line)
        if match:
            # 如果匹配到注释
            indent, code_part, comment = match.groups()
            # 将注释转换为 C++ 风格
            new_comment = f'/* {re.sub(r'//|\s+', '', comment).strip()} */'
            # 如果注释在代码之后，将注释放在上面
            if code_part.strip():
                new_lines.append(f'{indent}{new_comment}')
                new_lines.append(f'{indent}{code_part.rstrip()}')
            else:
                new_lines.append(f'{indent}{new_comment}')
        else:
            new_lines.append(line)

    return '\n'.join(new_lines)

originEncodeList = ['gbk', 'Big5', 'iso-latin-1-dos', 'gb2312']

if __name__ == '__main__':
    objFiles = getDirFiles('.')
    failList = []
    for f in objFiles:
        if f.endswith('.c') or f.endswith('.h'):
            with open(f, 'r', encoding='utf-8') as file:
                code = file.read()
            converted_code = convert_comments(code)
            with open(f.rsplit('.', 1)[0] + '_converted.' + f.rsplit('.', 1)[1], 'w', encoding='utf-8') as file:
                file.write(converted_code)

    print('All Done!')
