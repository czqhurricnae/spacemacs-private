# python -m pip install baidu-aip
from aip import AipOcr
import os
import sys
import subprocess

config = {
    'appId': '15533798',
    'apiKey': 'buM9xs666WpENtcOnxkWPxH0',
    'secretKey': 'cpvqXrREgNGpYYOdn4m29gmCGSZpjiGn',
}

client = AipOcr(**config)


def get_file_content(file):
    with open(file, 'rb') as fp:
        return fp.read()


def img_to_str(image_path):
    image = get_file_content(image_path)
    result = client.basicGeneral(image)
    if 'words_result' in result:
        return '\n'.join([w['words'] for w in result['words_result']])


def write_to_clipboard(output):
    process = subprocess.Popen('pbcopy',
                               env={'LANG': 'en_US.UTF-8'},
                               stdin=subprocess.PIPE)
    process.communicate(output.encode('utf-8'))


if __name__ == '__main__':
    img_path = sys.argv[1]
    result = img_to_str(img_path)
    write_to_clipboard(result)
