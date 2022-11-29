from pdf2docx import Converter
import PySimpleGUI as sg

def pdf2word(file_path):
    file_name = file_path.split('.')[0]
    docx_file = f'{file_name}.docx'


    p2w = Converter(file_path)
    p2w.convert(docx_file, start=0, end=None)
    p2w.close()

    return docx_file


def main():
    sg.theme('BlueMono')

    layout = [
            [sg.Text('PDF转Word', font=('微软雅黑', 12)),
            sg.Text('', key='filename', size=(50,1), font=('微软雅黑', 10), text_color = 'blue')],
            [sg.Output(size=(80, 10), font=('微软雅黑', 10))],
            [sg.FileBrowse('选择文件', key='file', target='filename'),sg.Button('开始转换'),sg.Button('退出')]
            ] 

    window = sg.Window('Yuuki', layout, font=('Times New Roman', 15), default_element_size=(50,1))

    while True:
        event, values = window.read()

        if event in (None, '退出'):
            break

        if event == '开始转换':
            if values['file'] and values['file'].split('.')[1]=='pdf':
                file_path = pdf2word(values['file'])
                print('\n'+'转换成功'+'\n')
                print('word文件位置:',file_path)

            else:
                print('未选取文件或文件非PDF格式\n请先选择文件')
    window.close()

main()


