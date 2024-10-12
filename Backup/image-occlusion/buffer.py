import os
import base64
from PIL import Image

from core.utils import *
from core.webengine import BrowserBuffer
from PyQt6.QtCore import QUrl


class AppBuffer(BrowserBuffer):
    def __init__(self, buffer_id, url, arguments):
        BrowserBuffer.__init__(self, buffer_id, url, arguments, False)
        self.url = url
        self.deck = arguments[0] if arguments else ""
        self.front = arguments[1] if arguments else ""
        self.load_index_html(__file__, "v3", True)

    def init_app(self):
        img = Image.open(self.url)
        width, height = img.size
        self.buffer_widget.eval_js_function("addImage", image_to_base64(self.url), height, width, self.deck, self.front)

    @PostGui()
    def handle_input_response(self, callback_tag, result_content):
        from inspect import signature

        handle_function_name = "handle_{}".format(callback_tag)
        if hasattr(self, handle_function_name):
            handle_function = getattr(self, handle_function_name)
            function_argument_number = len(signature(getattr(self, handle_function_name)).parameters)

            if function_argument_number == 1:
                handle_function(result_content)
            else:
                handle_function()


def image_to_base64(image_path):
    with open(image_path, "rb") as image_file:
        encoded_string = base64.b64encode(image_file.read())
    return "data:image/png;base64,{0}".format(encoded_string.decode('utf-8'))
