__author__ = 'shawnmehan'

import csv, re
import xml.etree.cElementTree


def write_csv(data):
    outfile = open('./Dataset1-Media-Example-EDGES.csv', 'wb')
    outwriter = csv.writer(outfile, delimiter=',')
    count = 0
    for d in data:  # iterate through each row
        outwriter.writerow(d)  # write the row
        count += 1
    print u"There were {0:d} lines processed in this run.".format(count)
    outfile.close()


def get_node_id(node, node_id):
    if (node.attrib.get('id')):
        node_id = node.attrib.get('id')  # set a new node_id if avail
    return(node_id)


def get_row(child, child_id, r, isNew):
    if isNew:
        r.append(child_id)
        isNew = False  # boolean to mark whether we have a new block
    else:
        r.append(child.text)
    return (r, isNew)


def recursive_child(parent):
    child_id = ''  # initialize the child_id
    new_row = True  # initialize boolean control
    row = []  # initialize row and build up data from <tr>
    for child in parent:
        child_id = get_node_id(child, child_id)
        row, new_row = get_row(child, child_id, row, new_row)
        if len(row) == 5:
            data.append(row)  # add previous complete row to output
            row = []  # reset row for new row.

# Uses a download of the data table page from github project 
# which I then use to extract all of the cells.
data = [['id', 'from', 'to', 'weight', 'type']]
with open('./data/Dataset1-Media-Exampes-EDGES.xml', mode='rt') as f:
    tree = xml.etree.ElementTree.parse(f)
for child in tree.iterfind('tbody/'):
    recursive_child(child)
write_csv(data)


