# Perform inference using Nkhotakota Wildife Reserve YOLOv4 model (NWR_YOLO_v1) on new images
## check darknet path in line 15

import time
import glob
import os
import getopt
import sys
import subprocess
import unicodedata
import argparse
from multiprocessing import Pool, Semaphore

#Set path to darknet installation
darknet_path = '/path/to/darknet/'

#Parse arguments
def parse_args():
    parser = argparse.ArgumentParser(description = 'Process photos in batch')
    parser.add_argument('base_dir', type=str, help='Base directory containing image subdirectories')
    parser.add_argument('image_folder', type=str, help='Subdirectory containing image files')
    parser.add_argument('--device', type=str, help='Optional: to use NVIDIA GPU, enter device number')
    return parser.parse_args()

args = parse_args()

#for NVIDIA GPUs (e.g., 0, 1, ...)
os.environ['CUDA_VISIBLE_DEVICES']=args.device

#read model files
data_path = "/NWR_YOLO_v1/obj.data"
cfg_path = "/NWR_YOLO_v1/obj.cfg"
weight_path = "/NWR_YOLO_v1/obj_final.weights"

#create commands
base_dir = args.base_dir
image_folder = args.image_folder
commands = []

def call_command(final_command):
    try:
        process = subprocess.Popen(final_command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        stdout, stderr = process.communicate()

        if process.returncode != 0:
            print("EXCEPTION!!!!")
            print('Error (Return Code {})'.format(process.returncode))
            print('Command: {}'.format(final_command))
            print('Output: {}'.format(stderr.decode('utf-8')))
            sys.exit(1)

    except Exception as e:
        print("EXCEPTION!!!!")
        print('Error: {}'.format(str(e)))
        sys.exit(1)


for dir in os.listdir(base_dir):
    path = base_dir + '/' + dir
    if os.path.isdir(path):
        for projectdir in os.listdir(path):
            path = base_dir + '/' + dir + '/' + projectdir
            if os.path.isdir(path):
                name = projectdir
                if name[:3] == args.image_folder:
                    text_path = base_dir + '/' + dir + '/' + name + '.txt'
                    output_path = base_dir + '/' + dir + '/OUT' + name + '.txt'
                
                    f = open(text_path, 'w')
                    for file in os.listdir(path):
                        abs_file = path + '/' + file
                        if os.path.isfile(abs_file):
                            f.write(abs_file + '\n')
                           
                    f.close()
                    cmd = "cd " + darknet_path + "; ./darknet detector test " + data_path + " " + cfg_path + " " + weight_path + " -dont_show -ext_output -out " + output_path + " < " + text_path
                    commands.append(cmd)


max_concurrency = 4
semaphore = Semaphore(max_concurrency)

def execute_command_with_semaphore(command):
    with semaphore:
        call_command(command)


# Number of processes to run in parallel
num_processes = len(commands)

# Create a process pool with the specified number of processes
start = time.time()

pool = Pool(processes=max_concurrency)

# Run the commands in parallel using the process pool
pool.map(execute_command_with_semaphore, commands)

# Close the pool and wait for all processes to finish
pool.close()
pool.join()

# # call_command(cmd)
# end = time.time()

# # print("TIME:")
# # print(end - start)

# f = open("time.txt", 'w')
# f.write("TIME: ")
# f.write(str(end - start))
# f.close()

# print("----------------------")



