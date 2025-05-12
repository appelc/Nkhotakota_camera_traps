# Create Njobvu-AI project from YOLO output file
## check path in line 12 

import os
import sqlite3
import json
import re
import argparse
from PIL import Image

#Set path to Njobvu-AI installation
njobvu_dir = '/path/to/Njobvu-AI-main'

#Parse arguments
def parse_args():
    parser = argparse.ArgumentParser(description = "Create Njobvu-AI projects from text files")
    parser.add_argument("base_dir", type=str, help = "Base directory containing image subdirectories")
    parser.add_argument("image_folder", type=str, help = "Subdirectory containing image files")
    parser.add_argument("username", type=str, help = "Njobvu-AI username to be used for this project")
    return parser.parse_args()
args = parse_args()

#Directories
projects_dir = njobvu_dir + '/public/projects/'
db_dir = njobvu_dir + '/db/'
classFile = 'NWR_YOLO_v1/classes.txt'
base_dir = args.base_dir
image_folder = args.image_folder

#Find output from inference script
label_path = base_dir + '/' + image_folder + '/OUT' + image_folder + '.txt'

#Users and specs
username = args.username
auto_save = '1' #0/1, no/yes
project_description = 'Manually created'


def custom_sort(value):
    #match = re.search(r'__(\d{4}-\d{2}-\d{2})__(\d+-\d+-\d+\(\d+\))', value)
    match = re.search(r'_(\d+)\.', value)
    if match:
        date = match.group(1)
        second_section = match.group(2)
        return (date, second_section)
        #print(match.group(1))
        #return match.group(1)
    else:
        return ('', '')


def get_dim(filepath):
    with Image.open(filepath) as img:
        return img.size


def createProject(projectName, folderPath, data):

    print('creating:')
    print({projectName})

    #Fill in list of classes
    #classList = set()
    classList = []
    f = open(classFile)
    lines = f.readlines()
    for line in lines:
        #l = line.strip().lower().replace(' ', '_')
        #classList.add(''.join(ch for ch in l if ch.isalnum()))
        classList.append(line.strip())

    f.close()

    #Create Initial folders
    fixedPath = projects_dir + username + '-' + projectName
    os.system('rm -rf ' + fixedPath)
    os.mkdir(fixedPath)
    os.mkdir(fixedPath + '/images')
    trainPath = fixedPath + '/training'

    os.mkdir(trainPath)
    os.mkdir(trainPath + '/logs')
    os.mkdir(trainPath + '/python')
    os.mkdir(trainPath + '/weights')

    open(trainPath + '/darknetPaths.txt', 'w')
    open(trainPath + '/Paths.txt', 'w')

    #db intialization
    db = sqlite3.connect(fixedPath + '/' + projectName + '.db')
    write = db.cursor()
    write.execute('''CREATE TABLE IF NOT EXISTS Classes (CName VARCHAR NOT NULL PRIMARY KEY)''')
    write.execute('''CREATE TABLE IF NOT EXISTS Images (IName VARCHAR NOT NULL PRIMARY KEY, reviewImage INTEGER NOT NULL DEFAULT 0)''')
    write.execute('''CREATE TABLE IF NOT EXISTS Labels (LID INTEGER PRIMARY KEY, CName VARCHAR NOT NULL, X INTEGER NOT NULL, Y INTEGER NOT NULL, W INTEGER NOT NULL, H INTEGER NOT NULL, IName VARCHAR NOT NULL, FOREIGN KEY(CName) REFERENCES Classes(CName), FOREIGN KEY(IName) REFERENCES Images(IName))''')
    write.execute("CREATE TABLE Validation (Confidence INTEGER NOT NULL, LID INTEGER NOT NULL PRIMARY KEY, CName VARCHAR NOT NULL, IName VARCHAR NOT NULL, FOREIGN KEY(LID) REFERENCES Labels(LID), FOREIGN KEY(IName) REFERENCES Images(IName), FOREIGN KEY(CName) REFERENCES Classes(CName))")

    #db class insertion
    for i,insert_class in enumerate(classList):
        insert_class = insert_class.strip()
        insert_class = insert_class.replace(' ', '_')
        insert_class = insert_class.replace('+', '+')
        if(insert_class != ''):
            #print(i, insert_class)
            write.execute("INSERT OR IGNORE INTO CLASSES (CName) VALUES ('" + insert_class + "')")

    #Insert Labels
    labelID = 0
    for line in data:
        filename, class_name, label_height, label_width, left_x, top_y, confidence = line

        if class_name not in classList:
            print("Inserting Class: ", class_name)
            classList.add(class_name)
            write.execute("INSERT OR IGNORE INTO CLASSES (CName) VALUES ('" + classList + "')")

        labelID += 1

        write.execute("INSERT INTO Labels (LID, CName, X, Y, W, H, IName) VALUES ('"+str(labelID)+"', '" + str(class_name) + "', '" + str(left_x) + "', '" + str(top_y) + "', '" + str(label_width) + "', '" + str(label_height) + "', '" + str(filename) + "')")
        write.execute("INSERT INTO Validation (Confidence, LID, CName, IName) VALUES ('" + str(confidence) + "', '" + str(labelID) +  "', '" + str(class_name) + "', '" + str(filename) + "')")

    f.close()

    #db and folder image insertion
    for image in sorted(os.listdir(folderPath), key=custom_sort):
        insert_image = image.strip()
        insert_image = insert_image.replace(' ', '_')
        insert_image = insert_image.replace('+', '_')
        os.symlink(folderPath + '/' + image, fixedPath + '/images/' + insert_image)
        write.execute("INSERT INTO Images (IName, reviewImage) VALUES ('" +  insert_image + "', '" + str(0) + "')")

    db.commit()
    db.close()

    #change permissions so labeling_tool can use images
    os.system('chown osulabel:osulabel ' + fixedPath)
    os.system('chown -R osulabel:osulabel ' + fixedPath + '/*')
    os.system('chgrp -R osulabel ' + fixedPath + '/*')

    #update manage.db database
    db = sqlite3.connect(db_dir + 'manage.db')
    write = db.cursor()
    write.execute("DELETE FROM Projects WHERE PName = '" + projectName + "'")
    write.execute("DELETE FROM Access WHERE PName = '" + projectName + "'")
    write.execute("INSERT OR IGNORE INTO Projects (PName, PDescription, AutoSave, Admin) VALUES ('" + projectName + "', '" + project_description + "', '" + auto_save +"', '" + username + "')")
    write.execute("INSERT OR IGNORE INTO Access (Username, PName, Admin) VALUES ('"+username+"', '"+projectName+"', '"+username+"')")
    db.commit()
    db.close()


#Run:
f = open(label_path)
data = json.load(f)

project_data = []
last_pname = ''

#read image dim of first entry 
first_img = data[0]
first_filename = first_img.get('filename')
dim = get_dim(first_filename)
print(dim)
img_width = dim[0]
img_height = dim[1]

#iterate through all entries
for line in data:
    root, nfs, fw, levi, yolo, year, base_folder, project_name, photo_name = line['filename'].split('/')

    if project_data and project_name != last_pname:
        createProject(last_project_name, base_dir + '/' + base_folder + '/' + last_pname, project_data)
        project_data = []


    for obj in line['objects']:
        rc = obj['relative_coordinates']
        label_width = img_width * rc['width']
        label_height = img_height * rc['height']
        left_x = (rc['center_x'] * img_width) - ((rc['width'] * img_width) / 2) 
        top_y = (rc['center_y'] * img_height) - ((rc['height'] * img_height) / 2)
        class_name = obj['name']
        filename = line['filename'].split('/')[-1]
        confidence = (float(obj['confidence']) * 100)

        project_data.append([photo_name, class_name, label_height, label_width, left_x, top_y, confidence])

    last_pname = project_name


createProject(last_pname, base_dir + '/' + base_folder + '/' + last_pname, project_data) #edge case catch last project

f.close()
