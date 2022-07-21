# -*- coding:utf-8 -*-
#!/usr/bin/env python3
import fitz

from typing import Tuple
import os

def convert_pdf2img(input_file: str, pages: Tuple = None, relative_dir: str = None, output_dir: str = None):
    """Converts pdf to image and generates a file by page"""
    # Open the document
    pdf_file = fitz.open(input_file)
    relative_files = []
    # Iterate throughout the pages
    for page_index in range(1, pdf_file.page_count + 1):
        if str(pages) != str(None) and str(pages) != str(""):
            if str(page_index) not in str(pages):
                continue
        # Select a page
        page = pdf_file[page_index - 1]
        rotate = int(0)
        # PDF Page is converted into a whole picture 1056*816 and then for each picture a screenshot is taken.
        # zoom = 1.33333333 -----> Image size = 1056*816
        # zoom = 2 ---> 2 * Default Resolution (text is clear, image text is hard to read)    = filesize small / Image size = 1584*1224
        # zoom = 4 ---> 4 * Default Resolution (text is clear, image text is barely readable) = filesize large
        # zoom = 8 ---> 8 * Default Resolution (text is clear, image text is readable) = filesize large
        zoom_x = 2
        zoom_y = 2
        # The zoom factor is equal to 2 in order to make text clear
        # Pre-rotate is to rotate if needed
        matrix = fitz.Matrix(zoom_x, zoom_y).prerotate(rotate)
        pix = page.get_pixmap(matrix=matrix, alpha=False)
        relative_file = os.path.join(relative_dir, f"{os.path.splitext(os.path.basename(input_file))[0]}_page{page_index}.png")
        output_file = os.path.join(output_dir, f"{os.path.splitext(os.path.basename(input_file))[0]}_page{page_index}.png")
        if not os.path.exists(output_file):
            pix.save(output_file)
        else:
            continue
        relative_files.append(relative_file)
    pdf_file.close()
    # summary = {
    #     "File": input_file, "Pages": "All" if str(pages) == str("") else str(pages), "Output File(s)": "\n" +  "\n".join(output_files)
    # }
    # # Printing Summary
    # print("## Summary ########################################################")
    # print("\n".join("{}: {}".format(i, j) for i, j in summary.items()))
    # print("###################################################################")
    print("\n\n" + "\n\n".join([f"[[file:{relative_file}]]" for relative_file in relative_files]))

if __name__ == "__main__":
    # import sys
    # input_file = sys.argv[1]
    # pages = sys.argv[2]
    # relative_dir = sys.argv[3]
    # output_dir = sys.argv[4]
    convert_pdf2img(input_file, pages, relative_dir, output_dir)
