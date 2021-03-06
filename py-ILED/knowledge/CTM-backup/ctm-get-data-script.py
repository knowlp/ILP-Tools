'''
Created on Jun 18, 2014

@author: nkatz
'''

import os,shutil,re,sys
from compiler.ast import flatten
from pymongo import Connection,MongoClient

""" Local paths with data in the flash drive """

narrative_path = '/media/Transcend/CTM-18-6-2014/Narrative/new-data-one-vehicle-17-6-2014/'
narrative_output = '/media/Transcend/CTM-18-6-2014/narrative-all.txt'
annotation_paths = ['/media/Transcend/CTM-18-6-2014/Annotation/1vehicle.txt', \
                    '/media/Transcend/CTM-18-6-2014/Annotation/1vehicle-partII.txt']
annotation_output = '/media/Transcend/CTM-18-6-2014/annotation-all.txt'


""" Cluster paths """
"""
narrative_path = '/home/nkatz/py-ILED/CTM/data/CTM-18-6-2014/Narrative/new-data-one-vehicle-17-6-2014/'
narrative_output = '/home/nkatz/py-ILED/CTM/data/CTM-18-6-2014/narrative-all.txt'
annotation_paths = ['/home/nkatz/py-ILED/CTM/data/CTM-18-6-2014/Annotation/1vehicle.txt', \
                    '/home/nkatz/py-ILED/CTM/data/CTM-18-6-2014/Annotation/1vehicle-partII.txt']
annotation_output = '/home/nkatz/py-ILED/CTM/data/CTM-18-6-2014/annotation-all.txt'
"""


""" The Target HLEs: """

HLEs = ['passenger_satisfaction=reducing','driving_quality=low','driving_style=unsafe','punctuality=punctual','punctuality=non_punctual', \
         'sharp_turn=sharp','sharp_turn=very_sharp','abrupt_acceleration=abrupt','abrupt_acceleration=very_abrupt','abrupt_deceleration=abrupt',\
         'abrupt_deceleration=very_abrupt','passenger_density=high','noise_level=high','internal_temperature=very_cold',]

dict = {}

#flatten = lambda x: [item for sublist in x for item in sublist]
get_full_path = lambda x,y,z:'%s%s/%s'%(x,y,z)
no_good_line = lambda x: True if x.replace(" ","") == '' or ':-' in x else False 
atom_join = lambda list: map(lambda x: x+'@',list)







def pre_process_data(step):
    global dict
    
    
    """ Step is used to generate windows. E.g for step = 2, the interval [0,10] will be sliced to [1,2],[2,4],[4,6]...
     For step=1 each data point is taken separately. """
    
    """ Handle narrative first """
    
    dirs = [d for  _, d, _ in os.walk(narrative_path) if d != []][0]
    # Sort them by y[0] where y = x.split('-') and x is of the form 1000-2000 
    dirs = sorted(dirs, key = lambda x: int(x.split('-')[0]))
    ## this won't work I can't get the path to the file to read it like this
    ## files = flatten([[f for _,_,f in os.walk(narrative_path+dir)][0] for dir in dirs])
    all_lles = []
    for dir in dirs:
        files = [f for _,_,f in os.walk(narrative_path+dir)][0] 
        #print(files)
        for file in files:
            with open(get_full_path(narrative_path,dir,file)) as f:
                lines = [l.replace(" ","") for l in f.read().splitlines() if not no_good_line(l)]
                for line in lines:
                    lle = line[0:len(line)-2].split('assert(')[1].replace('happensAtIE','happensAt')  
                    lle = lle.replace('start(abrupt_acceleration,','abrupt_acceleration_start(')
                    lle = lle.replace('end(abrupt_acceleration,','abrupt_acceleration_end(')
                    
                    lle = lle.replace('start(abrupt_deceleration,','abrupt_deceleration_start(')
                    lle = lle.replace('end(abrupt_deceleration,','abrupt_deceleration_end(')  
                   
                    lle = lle.replace('start(sharp_turn,','sharp_turn_start(')
                    lle = lle.replace('end(sharp_turn,','sharp_turn_end(')
                      
                    lle = lle.replace('75','id75',1)  
                      
                    # find the time point to use as key
                    time = re.findall('[0-9]+\)',lle)[0].split(')')[0]
                    
                    # see if it contains a bus stop id. If it does, then the id has to be quoted in order 
                    # for the ASP program to compile (the id is of the form e.g 0034)
                    if any(x in lle for x in ['stop_enter','stop_leave']):
                        m = re.findall(',\d+,',lle)
                        if m:
                            stripped = map(lambda x: x.replace(',','').strip(),m)
                            f = lambda x,y: str(x).replace(str(y),"\""+str(y)+"\"")
                            lle = f(lle,stripped[0])
                    
                    
                    all_lles.append(str(time)+' '+lle)
                    
                    ## ADD to the global dictionary with time as the key
                    update_example_dict(lle,None,time)
                         
    #_f = open(narrative_output,'w')
    #_f.write('\n'.join(all_lles))
    print('All LLEs: ',len(all_lles))       
    #_f.close() 
    
    """ ...next handle the annotation """
    
    an = []
    for file in annotation_paths:
        f = open(file,'r')
        # get HLE names only (drop the values)
        _names = map(lambda x: x.split('=')[0], HLEs) 
        # the lines that contain only the names (to use them as guides for the interval lines that follow)
        lines = filter(lambda x: True if x.strip()!='' else False,[l.strip() for l in f.read().splitlines()])
        #guides =  map(lambda x,y: y.index(x), filter(lambda x: True if any(y in x for y in _names) else False, lines), lines )
        guides =  filter(lambda x: True if any(y in x for y in _names) else False, lines)
        index_tuples = [(x,lines.index(x)) for x in guides]
        interval_tuples = [(x,lines[int(y+1)]) for (x,y) in index_tuples]
        g1 = lambda x: x.split('holdsFor(')[1].split('(')[0]
        g2 = lambda x: x.split('=')[1].split(',')[0]
        g3 = lambda x: re.findall('\(\d+,\d+\)',x)
        t = map(lambda (x,y): ((g1(x),g2(x)),g3(y)), interval_tuples)
        form_term = lambda (e,v),(tstart,tend): \
             ['%s holdsAt(%s(id75,bus,%s),%s)'%(m,e,v,m) for m in range(tstart-1,tend-1)] if ('passenger_satisfaction' in e\
             or 'driving_quality' in e) else \
             ['%s holdsAt(%s(id75,bus,%s),%s)'%(m,e,v,m) for m in range(tstart,tend)] 
        
        # not for driving_style
        
        #form_term = lambda (e,v),(tstart,tend): ['%s holdsAt(fluent(%s,75,bus,%s),%s)'%(m,e,v,m) for m in range(tstart,tend)] 
        
        to_int_tuples = lambda x: (int(noinf(x.split('(')[1].split(')')[0].split(',')[0])),int(noinf(x.split('(')[1].split(')')[0].split(',')[1])))
        annotation = [[form_term((e,v),to_int_tuples(z)) for z in k] for ((e,v),k) in t]
        an.append(annotation)
    an = flatten(an)
    print('all HLEss are: ',len(an))    
    #with open(annotation_output,'w') as af:
    #    af.write('\n'.join(an))
    #af.close()
    for item in an:
        update_example_dict(None,item,None)  
    
    ## Finally get all entries from the dictionary and populate mongo DB

    db_name = 'CTM-granularity-'+(str(step))
    collection_name = 'examples' 
    connection = Connection()
    connection.drop_database(db_name) ## clear database
    db = connection[db_name] ## get a new one
    collection = db[collection_name]

    for i in range(0,50000,step):
        anot_j = []
        nar_j = []
        _innert = []
        for j in range(i-step,i+1):
            if j in dict:
                if 'pos' in dict[j]:
                    anot_j.extend(dict[j]['pos'])
                if 'nar' in dict[j]:    
                    nar_j.extend(dict[j]['nar'])
                if i-step-1 in dict:
                    if 'pos' in dict[i-step-1]:
                        _innert.extend(dict[i-step-1]['pos'])    
        #t = lambda list: reduce(lambda x,y: str(x)+'@'+str(y),list) if list != [] else ''
        t = lambda x: x
        nar_j.append('starttime(%s)'%(str(i-step)))
        try:
            post = {'example':j,'pos':t(anot_j),'nar':t(nar_j),'neg':'','innert':t(_innert),'metadata':''}
            
            #print(i-step,i,post)
            print(i-step,i)
        except TypeError:
            print('TypeError at')
            print(anot_j)
            print(nar_j)
            print(_innert)
            sys.exit()    
        examples = db.examples
        examples.insert(post)        
            

    
                    
def noinf(num): 
    if num == 'inf':
        num = 50000
    return num         
    

def update_example_dict(lle_atom,hle_atom,time):
    global dict
    if lle_atom != None and time != None:
        try:
            time = int(time)
            if (int(time)) in dict:
                if 'nar' in dict[time]:
                    dict[time]['nar'].append(lle_atom)
                else:
                    dict[time]['nar'] = []
                    dict[time]['nar'].append(lle_atom)     
            else:
                dict[time] = {}
                # Append the lle to narrative
                dict[time]['nar'] = []
                dict[time]['nar'].append(lle_atom)  
                ## ... and set all the things that if not set fetching examples will probably crash on the java side.
                ## Set them here because lles are preprocessed first 
                dict[time]['neg'] = []
                dict[time]['meta'] = []
        except:
            print(lle_atom)
            sys.exit()        
    if hle_atom != None:
        s = hle_atom.split()
        time = s[0].strip()
        hle = s[1].strip()
        time = int(time)
        if (int(time)) in dict:
            if 'pos' in dict[time]:
                dict[time]['pos'].append(hle)
            else:
                dict[time]['pos'] = []
                dict[time]['pos'].append(hle)     
        else:
            dict[time] = {}
            # Append the hle list to the annotation
            dict[time]['pos'] = []
            dict[time]['pos'].append(hle)                       
    
         
 


if __name__ == '__main__':
    #get_narrative()
    #get_annotation()
    #store(10)
    pre_process_data(200)

    
    
    
    
    
    
    
