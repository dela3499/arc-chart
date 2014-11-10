import json

class ArcChart(object):

    def __init__(self, original_string):
        # original string
        self.org = original_string
        # identical substrings
        self.substrings = self.get_repeated_substring() 
        # non-overlaiing identical substrings
        ArcChart._remove_overlapping_substrings(self.substrings)
        # list of consecutive, non-overlapping pairs
        self.matching_pairs = ArcChart.get_consecutive_pairs(self.substrings)
        # list of maximal, consecutive, non-overlapping pairs
        ArcChart._remove_nonmaximal_pairs(self.matching_pairs)        

    class Pair(object):
        def __init__(self, first_substring, second_substring):
            self.value = (first_substring, second_substring)
        def contains(self, pair):
            """ returns true if the calling object contains the argument object """
            def _1contains2(substring1, substring2):
                x_start = substring1[0]
                x_end = substring1[1]
                y_start = substring2[0]
                y_end = substring2[1]
                return x_start <= y_start and x_end >= y_end
            pair1 = self.value
            pair2 = pair.value
            for i in xrange(2):
                if not ( _1contains2( pair1[i], pair2[i] ) ):
                    return False
            return True

    def export_json_format_pairs(self, file_name, pairs=None):
        if not pairs:
            pairs = self.matching_pairs
        list_of_pairs = []
        for each_pair in pairs:
            start1 = each_pair.value[0][0]
            start2 = each_pair.value[1][0]
            length = each_pair.value[0][1] - start1 
            pair = {'a': start1, 'b': start2,\
                    'n': length}
            list_of_pairs.append( pair )
        with open(file_name, 'w') as f:
            json.dump( list_of_pairs, f) 

    def get_matching_pairs_in_strings(self):
        retlist = []
        for each_pair in self.matching_pairs:
            str1_start = each_pair.value[0][0]
            str1_end = each_pair.value[0][1]
            str2_start = each_pair.value[1][0]
            str2_end = each_pair.value[1][1]
            retlist.append( ( self.org[str1_start:str1_end], self.org[str2_start:str2_end]))
        return retlist


    @staticmethod
    def _conver_format(file_name):
        content = ''
        with open(file_name) as f:
            content = f.read()
            content = content.replace('"', '')
            content = content.replace(':', '=')
        with open(file_name, 'w') as f:
            f.write(content)            


    def get_substrings(self):
        """ returns all identical substrings of the original string """
        s = self.org
    	all_substrings = {} # store all repeated substrings here in a dictionary of lists of tuples
    	for index in range(0,len(s)):
    		for substring_length in range(1, len(s)-index+1):
    			start = index
    			end = index+substring_length
    			substring = s[start:end]
    			if substring not in all_substrings:
    				all_substrings[substring] = [(start,end)]
    			else:
    				all_substrings[substring].append((start,end))
    	return all_substrings
    
    def get_repeated_substring(self):
        s = self.org
    	return dict((k,v) for k,v in self.get_substrings().iteritems() if len(v) > 1)

    @staticmethod
    def get_consecutive_pairs(dic):
        """ 
            returns a list of consecutive pairs from the given dictionary dic.
            dic should be pre-processed so that it does not contain any ocerlapping pairs
        """
        retlist = []        
        for each_key in dic.keys():
            sublist = sorted(dic[each_key])
            if ( len(sublist) > 1 ):
                for i in range(len(sublist)-1):
                    retlist.append( ArcChart.Pair(sublist[i], sublist[i+1]) )
        return retlist

    @staticmethod
    def _remove_overlapping_substrings(dic):
        for each_key in dic.keys():
            dic[each_key].sort()
            sublist = dic[each_key]
            for i in range(len(sublist)-1):
                to_be_removed = []
                for j in range(i+1, len(sublist)):
                    if ArcChart.is_overlapping(sublist[i], sublist[j]):
                        to_be_removed.append(sublist[j])
                for each_element in to_be_removed:
                    try:
                        sublist.remove(each_element) 
                    except ValueError:
                        pass

    @staticmethod
    def is_overlapping(x, y):
        """take tuples of form (start,end) that denote start and end points for
        each instance of a repeated substring
        return True if instances overlap (either the start or endpoint of the
        second instance lie between the start and endpoint of the first instance)"""
        
        def is_between(x,y1,y2):
            """return true if x is between y1 and y2"""
            if x > y1 and x < y2:
                return True
            else:
                return False        
        
        if is_between(y[0],x[0],x[1]) or is_between(y[1],x[0],x[1]):
            return True
        else:
            return False

    @staticmethod
    def _remove_nonmaximal_pairs(pairs):
        for pair1 in pairs[:]: 
            removable_pairs = []
            for pair2 in pairs:
                if ( pair1 is not pair2 ):
                    if ( pair1.contains(pair2) ):
                        pairs.remove(pair2)
 
