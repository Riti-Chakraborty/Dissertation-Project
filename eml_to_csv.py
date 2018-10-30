
# coding: utf-8

# # Ref: https://github.com/lovasoa/eml2csv/blob/master/eml2csv.py
# This code has been taken from the above link. It helps in extracting the headers of each .eml files and storing them in a .csv file.
# To execute this code it is necessary to have a Unix/Linus operating system as it doesnot work in windows.

# to run this code on linux/unix prompt the following script needs to be used

# python3 eml_to_csv.py *.eml outputfilename.csv


import email

def email_to_dict(mail):
    return {
        k.capitalize(): str(email.header.make_header(email.header.decode_header(s)))
        for k,s in mail.items()
    }

def file_to_dict(fname):
    sub = fname.split('-', 1)[0]
    with open(fname, 'r') as f:
        try: return email_to_dict(email.message_from_file(f))
        except: return {"Subject": sub}

def print_line(l):
    for s in l:
        print('"' + s.replace('"', '""').replace('\n', ' ') + '"', end=',')
    print("")

def files_to_csv(fnames):
    headers = dict()
    d = []
    for fname in fnames:
        vals = file_to_dict(fname)
        for header in vals.keys(): headers[header] = headers.get(header,0) + 1
        d.append(vals)
    headers = sorted(headers, key=headers.get, reverse=True)
    print_line(headers)
    for mail in d:
        print_line(map(lambda x: mail.get(x, ""), headers))

if __name__ == '__main__':
    import sys
    files_to_csv(sys.argv[1:])

