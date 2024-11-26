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

def convert_to_fullwidth_punctuations(text):
    # 定义中文字符的正则表达式
    cjk_pattern = r'([\u4E00-\u9FCC\u3400-\u4DB5\uFA0E\uFA0F\uFA11\uFA13\uFA14\uFA1F\uFA21\uFA23\uFA24\uFA27-\uFA29]|[\ud840-\ud868][\udc00-\udfff]|\ud869[\udc00-\uded6\udf00-\udfff]|[\ud86a-\ud86c][\udc00-\udfff]|\ud86d[\udc00-\udf34\udf40-\udfff]|\ud86e[\udc00-\udc1d])'

    # 定义中文字符前的半角标点符号及其对应的全角标点符号
    punct_before_cjk = {
        '"': '“',
        '(': '（',
        "'": '‘'
    }

    # 定义中文字符后的半角标点符号及其对应的全角标点符号
    punct_after_cjk = {
        '.': '。',
        ',': '，',
        '!': '！',
        '?': '？',
        ':': '：',
        ';': '；',
        '"': '”',
        ')': '）'
    }

    # 替换中文字符前的半角标点符号
    for punct_half, punct_full in punct_before_cjk.items():
        pattern = re.compile(re.escape(punct_half) + cjk_pattern)
        text = pattern.sub(punct_full + r'\1', text)

    # 替换中文字符后的半角标点符号
    for punct_half, punct_full in punct_after_cjk.items():
        pattern = re.compile(cjk_pattern + re.escape(punct_half))
        text = pattern.sub(r'\1' + punct_full, text)

    return text

def convert_punctuation(text):
    # 定义非中文标点符号到中文标点符号的映射
    punctuation_map = {
        ',': '，',
        '.': '。',
        '!': '！',
        # '?': '？',
        ':': '：',
        # ';': '；',
        # '(': '（',
        # ')': '）',
        # '[': '【',
        # ']': '】',
        # '{': '｛',
        # '}': '｝',
        # '<': '《',
        # '>': '》',
        '"': '“',
        "'": '‘',
        '-': '－',
        # '_': '＿',
        '/': '／',
        '\\': '＼',
        '@': '＠',
        '#': '＃',
        '$': '＄',
        '%': '％',
        '^': '＾',
        # '&': '＆',
        # '*': '＊',
        # '+': '＋',
        # '=': '＝',
        # '~': '～',
        # '`': '｀',
        # '|': '｜'
    }

    # 使用正则表达式替换非中文标点符号
    def replace_punctuation(match):
        char = match.group(0)
        return punctuation_map.get(char, char)

    pattern = re.compile('|'.join(re.escape(p) for p in punctuation_map.keys()))
    converted_text = pattern.sub(replace_punctuation, text)

    return converted_text

def convert_comments_style(code):
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
            new_raw_comment = re.sub(r'//', '', comment).strip()
            # new_comment = f'/* {convert_punctuation(new_raw_comment)} */'
            new_comment = f'/* {convert_to_fullwidth_punctuations(new_raw_comment)} */'
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

def hex_to_lower(text):
    # 定义正则表达式模式，匹配以 0x 或 0X 开头的十六进制数字
    pattern = r'(0[xX])([0-9A-Fa-f]+)'

    # 使用 re.sub 进行替换，将匹配到的十六进制数字中的大写字母转换为小写
    result = re.sub(pattern, lambda match: match.group(1).lower() + match.group(2).lower(), text)

    return result

if __name__ == '__main__':
    if len(sys.argv) < 2:
        objFiles = getDirFiles('.')
        failList = []
        for f in objFiles:
            if f.endswith('.c') or f.endswith('.h'):
                with open(f, 'r', encoding='utf-8') as file:
                    code = file.read()
                converted_text = convert_comments_style(code)
                converted_text = hex_to_lower(converted_text)
                with open(f.rsplit('.', 1)[0] + '_converted.' + f.rsplit('.', 1)[1], 'w', encoding='utf-8') as file:
                    file.write(converted_text)

        print('All Done!')
    else:
        for f in sys.argv[1:]:
            if f.endswith('.c') or f.endswith('.h'):
                with open(f, 'r', encoding='utf-8') as file:
                    code = file.read()
                    converted_text = convert_comments_style(code)
                    converted_text = hex_to_lower(converted_text)
                with open(f.rsplit('.', 1)[0] + '_converted.' + f.rsplit('.', 1)[1], 'w', encoding='utf-8') as file:
                    file.write(converted_text)

        print('All Done!')
