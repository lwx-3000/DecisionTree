# decision_tree.py
# ---------
# Licensing Information:  You are free to use or extend these projects for
# personal and educational purposes provided that (1) you do not distribute
# or publish solutions, (2) you retain this notice, and (3) you provide clear
# attribution to UT Dallas, including a link to http://cs.utdallas.edu.
#
# This file is part of Homework for CS6375: Machine Learning.
# Gautam Kunapuli (gautam.kunapuli@utdallas.edu)
# Sriraam Natarajan (sriraam.natarajan@utdallas.edu),
# Anjum Chida (anjum.chida@utdallas.edu)
#
#
# INSTRUCTIONS:
# ------------
# 1. This file contains a skeleton for implementing the ID3 algorithm for
# Decision Trees. Insert your code into the various functions that have the
# comment "INSERT YOUR CODE HERE".
#
# 2. Do NOT modify the classes or functions that have the comment "DO NOT
# MODIFY THIS FUNCTION".
#
# 3. Do not modify the function headers for ANY of the functions.
#
# 4. You may add any other helper functions you feel you may need to print,
# visualize, test, or save the data and results. However, you MAY NOT utilize
# the package scikit-learn OR ANY OTHER machine learning package in THIS file.

import numpy as np
import os
import graphviz
import matplotlib.pyplot as plt
from sklearn.metrics import confusion_matrix
from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import train_test_split
from sklearn import preprocessing

def partition(x):
    """
    Partition the column vector x into subsets indexed by its unique values (v1, ... vk)

    Returns a dictionary of the form
    { v1: indices of x == v1,
      v2: indices of x == v2,
      ...
      vk: indices of x == vk }, where [v1, ... vk] are all the unique values in the vector z.
    """

    # INSERT YOUR CODE HERE
    xsubset = {}
    for i in range(len(x)):
        if xsubset.get(x[i]) != None:
            xsubset[x[i]].append(i)
        else:
            xsubset[x[i]] = [i]
    return xsubset

def entropy(y):
    """
    Compute the entropy of a vector y by considering the counts of the unique values (v1, ... vk), in z

    Returns the entropy of z: H(z) = p(z=v1) log2(p(z=v1)) + ... + p(z=vk) log2(p(z=vk))
    """

    # INSERT YOUR CODE HERE
    xsubset = partition(y)
    entro = 0
    for i in xsubset.keys():
        p = len(xsubset.get(i)) / len(y)
        entro -= p * np.log2(p)
    return entro


def mutual_information(x, y):
    """
    Compute the mutual information between a data column (x) and the labels (y). The data column is a single attribute
    over all the examples (n x 1). Mutual information is the difference between the entropy BEFORE the split set, and
    the weighted-average entropy of EACH possible split.

    Returns the mutual information: I(x, y) = H(y) - H(y | x)
    """

    # INSERT YOUR CODE HERE
    entro = entropy(y)
    xsubset = partition(x)
    for i in xsubset.values():
        l = []
        for j in i:
            l.append(y[j])
        entro -= len(ls) / len(y) * entropy(ls)
    return entro
    
def ttest_split(x, y, pair):
    x_true = []
    y_true = []
    x_false = []
    y_false = []
    for i in range(len(x)):
        if x[i][pair[0]] == pair[1]:
            x_true.append(x[i])
            y_true.append(y[i])
        else:
            x_false.append(x[i])
            y_false.append(y[i])
    return x_true, y_true, x_false, y_false


def id3(x, y, attribute_value_pairs=None, depth=0, max_depth=5):
    """
    Implements the classical ID3 algorithm given training data (x), training labels (y) and an array of
    attribute-value pairs to consider. This is a recursive algorithm that depends on three termination conditions
        1. If the entire set of labels (y) is pure (all y = only 0 or only 1), then return that label
        2. If the set of attribute-value pairs is empty (there is nothing to split on), then return the most common
           value of y (majority label)
        3. If the max_depth is reached (pre-pruning bias), then return the most common value of y (majority label)
    Otherwise the algorithm selects the next best attribute-value pair using INFORMATION GAIN as the splitting criterion
    and partitions the data set based on the values of that attribute before the next recursive call to ID3.

    The tree we learn is a BINARY tree, which means that every node has only two branches. The splitting criterion has
    to be chosen from among all possible attribute-value pairs. That is, for a problem with two features/attributes x1
    (taking values a, b, c) and x2 (taking values d, e), the initial attribute value pair list is a list of all pairs of
    attributes with their corresponding values:
    [(x1, a),
     (x1, b),
     (x1, c),
     (x2, d),
     (x2, e)]
     If we select (x2, d) as the best attribute-value pair, then the new decision node becomes: [ (x2 == d)? ] and
     the attribute-value pair (x2, d) is removed from the list of attribute_value_pairs.

    The tree is stored as a nested dictionary, where each entry is of the form
                    (attribute_index, attribute_value, True/False): subtree
    * The (attribute_index, attribute_value) determines the splitting criterion of the current node. For example, (4, 2)
    indicates that we test if (x4 == 2) at the current node.
    * The subtree itself can be nested dictionary, or a single label (leaf node).
    * Leaf nodes are (majority) class labels

    Returns a decision tree represented as a nested dictionary, for example
    {(4, 1, False):
        {(0, 1, False):
            {(1, 1, False): 1,
             (1, 1, True): 0},
         (0, 1, True):
            {(1, 1, False): 0,
             (1, 1, True): 1}},
     (4, 1, True): 1}
    """

    # INSERT YOUR CODE HERE. NOTE: THIS IS A RECURSIVE FUNCTION.
    tree = {}
    if attribute_value_pairs == None:
        attribute_value_pairs = []
        for row in x:
            for i in range(len(row)):
                if (i, row[i]) not in attribute_value_pairs:
                    attribute_value_pairs.append((i, row[i]))
    if (depth == max_depth) or (len(attribute_value_pairs) == 0):
        return 1 if 2 * sum(y) >= len(y) else 0
    if sum(y) == len(y):
        return 1
    if sum(y) == 0:
        return 0
    best_gain = -1
    best_pair = ()
    x_true = []
    y_true = []
    x_false = []
    y_false = []
    for pair in attribute_value_pairs:
        e = entropy(y)
        curr_x_true, curr_y_true, curr_x_false, curr_y_false = ttest_split(x, y, pair)
        curr_info_gain = e - entropy(curr_y_true) - entropy(curr_y_false)
        if curr_info_gain > best_gain:
            best_gain = curr_info_gain
            best_pair = pair
            x_true = curr_x_true
            y_true = curr_y_true
            x_false = curr_x_false
            y_false = curr_y_false
            
    attribute_value_pairs.remove(best_pair)
    tree.update({(best_pair[0], best_pair[1], True): id3(x_true, y_true, attribute_value_pairs, depth+1, max_depth)})
    tree.update({(best_pair[0], best_pair[1], False): id3(x_false, y_false, attribute_value_pairs, depth+1, max_depth)})
    return tree


def predict_example(x, tree):
    """
    Predicts the classification label for a single example x using tree by recursively descending the tree until
    a label/leaf node is reached.

    Returns the predicted label of x according to tree
    """

    # INSERT YOUR CODE HERE. NOTE: THIS IS A RECURSIVE FUNCTION.
    if type(tree) == dict:
        for i in tree:
            if (x[i[0]] == i[1] and i[2] == True) or (x[i[0]] != i[1] and i[2] == False):
                return predict_example(x, tree.get(i))
    return tree;

def compute_error(y_true, y_pred):
    """
    Computes the average error between the true labels (y_true) and the predicted labels (y_pred)

    Returns the error = (1/n) * sum(y_true != y_pred)
    """

    # INSERT YOUR CODE HERE
    total = 0
    for i in range(len(y_true)):
        if y_true[i] != y_pred[i]:
            total += 1
    error = total / len(y_true)
    return error


def pretty_print(tree, depth=0):
    """
    Pretty prints the decision tree to the console. Use print(tree) to print the raw nested dictionary representation
    DO NOT MODIFY THIS FUNCTION!
    """
    if depth == 0:
        print('TREE')

    for index, split_criterion in enumerate(tree):
        sub_trees = tree[split_criterion]

        # Print the current node: split criterion
        print('|\t' * depth, end='')
        print('+-- [SPLIT: x{0} = {1} {2}]'.format(split_criterion[0], split_criterion[1], split_criterion[2]))

        # Print the children
        if type(sub_trees) is dict:
            pretty_print(sub_trees, depth + 1)
        else:
            print('|\t' * (depth + 1), end='')
            print('+-- [LABEL = {0}]'.format(sub_trees))


def render_dot_file(dot_string, save_file, image_format='png'):
    """
    Uses GraphViz to render a dot file. The dot file can be generated using
        * sklearn.tree.export_graphviz()' for decision trees produced by scikit-learn
        * to_graphviz() (function is in this file) for decision trees produced by  your code.
    DO NOT MODIFY THIS FUNCTION!
    """
    if type(dot_string).__name__ != 'str':
        raise TypeError('visualize() requires a string representation of a decision tree.\nUse tree.export_graphviz()'
                        'for decision trees produced by scikit-learn and to_graphviz() for decision trees produced by'
                        'your code.\n')

    # Set path to your GraphViz executable here
    os.environ["PATH"] += os.pathsep + 'C:/Program Files (x86)/Graphviz2.38/bin/'
    graph = graphviz.Source(dot_string)
    graph.format = image_format
    graph.render(save_file, view=True)


def to_graphviz(tree, dot_string='', uid=-1, depth=0):
    """
    Converts a tree to DOT format for use with visualize/GraphViz
    DO NOT MODIFY THIS FUNCTION!
    """

    uid += 1       # Running index of node ids across recursion
    node_id = uid  # Node id of this node

    if depth == 0:
        dot_string += 'digraph TREE {\n'

    for split_criterion in tree:
        sub_trees = tree[split_criterion]
        attribute_index = split_criterion[0]
        attribute_value = split_criterion[1]
        split_decision = split_criterion[2]

        if not split_decision:
            # Alphabetically, False comes first
            dot_string += '    node{0} [label="x{1} = {2}?"];\n'.format(node_id, attribute_index, attribute_value)

        if type(sub_trees) is dict:
            if not split_decision:
                dot_string, right_child, uid = to_graphviz(sub_trees, dot_string=dot_string, uid=uid, depth=depth + 1)
                dot_string += '    node{0} -> node{1} [label="False"];\n'.format(node_id, right_child)
            else:
                dot_string, left_child, uid = to_graphviz(sub_trees, dot_string=dot_string, uid=uid, depth=depth + 1)
                dot_string += '    node{0} -> node{1} [label="True"];\n'.format(node_id, left_child)

        else:
            uid += 1
            dot_string += '    node{0} [label="y = {1}"];\n'.format(uid, sub_trees)
            if not split_decision:
                dot_string += '    node{0} -> node{1} [label="False"];\n'.format(node_id, uid)
            else:
                dot_string += '    node{0} -> node{1} [label="True"];\n'.format(node_id, uid)

    if depth == 0:
        dot_string += '}\n'
        return dot_string
    else:
        return dot_string, node_id, uid


if __name__ == '__main__':
    # Load the training data
    M = np.genfromtxt('./monks-1.train', missing_values=0, skip_header=0, delimiter=',', dtype=int)
    ytrn = M[:, 0]
    Xtrn = M[:, 1:]

    # Load the test data
    M = np.genfromtxt('./monks-1.test', missing_values=0, skip_header=0, delimiter=',', dtype=int)
    ytst = M[:, 0]
    Xtst = M[:, 1:]

    # Learn a decision tree of depth 3
    decision_tree = id3(Xtrn, ytrn, max_depth=3)

    # Pretty print it to console
    pretty_print(decision_tree)

    # Visualize the tree and save it as a PNG image
    dot_str = to_graphviz(decision_tree)
    render_dot_file(dot_str, './my_learned_tree')

    # Compute the test error
    y_pred = [predict_example(x, decision_tree) for x in Xtst]
    tst_err = compute_error(ytst, y_pred)

    print('Test Error = {0:4.2f}%.'.format(tst_err * 100))
    
    
def questionB():
    for monk_set in range(1, 4):
        M_trn = np.genfromtxt('./monks-' + str(monk_set) + '.train', missing_values=0, skip_header=0, delimiter=',', dtype=int)
        M_tst = np.genfromtxt('./monks-' + str(monk_set) + '.test', missing_values=0, skip_header=0, delimiter=',', dtype=int)
        ytrn = M_trn[:, 0]
        Xtrn = M_trn[:, 1:]
        ytst = M_tst[:, 0]
        Xtst = M_tst[:, 1:]
        trn_errs = []
        tst_errs = []
        for d in range(1, 11):
            descision_tree = id3(Xtrn, ytrn, max_depth=d)
            y_pred_trn = [predict_example(x, descision_tree) for x in Xtrn]
            trn_err = compute_error(ytrn, y_pred_trn)
            trn_errs.append(trn_err)
            
            y_pred_tst = [predict_example(x, descision_tree) for x in Xtst]
            tst_err = compute_error(ytst, y_pred_tst)
            tst_errs.append(tst_err)
        plt.figure()
        plt.title('Monk-%i' %monk_set, fontsize=18)
        plt.xlabel('Tree Level', fontsize=16)
        plt.ylabel('Train_Test_Error', fontsize=16)
        plt.plot(np.arange(1,11), trn_errs, marker='o')
        plt.plot(np.arange(1,11), tst_errs, marker='x')
        plt.legend(['Train_Error', 'Test_Error'])
        plt.xticks(np.arange(0,12))
        plt.axis([0.5,10.5,0,1])
        
def questionC():
    M_trn = np.genfromtxt('./monks-1.train', missing_values=0, skip_header=0, delimiter=',', dtype=int)
    ytrn = M_trn[:, 0]
    Xtrn = M_trn[:, 1:]
    M_tst = np.genfromtxt('./monks-1.test', missing_values=0, skip_header=0, delimiter=',', dtype=int)
    ytst = M_tst[:, 0]
    Xtst = M_tst[:, 1:]
    for d in [1,3,5]:
        decision_tree = id3(Xtrn, ytrn, max_depth=d)
        dot_str = to_graphviz(decision_tree)
        render_dot_file(dot_str, './question_C_depth=' + str(d))
        y_pred = [predict_example(x, decision_tree) for x in Xtst]
        print("Confusion Matrix for depth = " + str(d))
        print(confusion_matrix(ytst, y_pred))
        
def questionD():
    M_trn = np.genfromtxt('./monks-1.train', missing_values=0, skip_header=0, delimiter=',', dtype=int)
    ytrn = M_trn[:, 0]
    Xtrn = M_trn[:, 1:]
    M_tst = np.genfromtxt('./monks-1.test', missing_values=0, skip_header=0, delimiter=',', dtype=int)
    ytst = M_tst[:, 0]
    Xtst = M_tst[:, 1:]
    for d in [1,3,5]:
        model = DecisionTreeClassifier(criterion='entropy', max_depth=d)
        model.fit(Xtrn, ytrn)
        y_pred = model.predict(Xtst)
        print("Confusion Matrix for depth = " + str(d))
        print(confusion_matrix(ytst, y_pred))
        
def questionE():
    M = np.genfromtxt('./tic-tac-toe.data', missing_values=0, skip_header=0, delimiter=',', dtype=str)
    X = M[:,:-1]
    y = M[:, -1]
    Xtrn, Xtst, ytrn, ytst = train_test_split(X, y, test_size=0.3, random_state=42)
    Xtrn_t = Xtrn
    Xtst_t = Xtst
    le_x = preprocessing.LabelEncoder()
    le_x.fit(['b', 'x', 'o'])
    for i in range(len(X[0])):
        Xtrn_t[:, i] = le_x.transform(Xtrn[:, i])
        Xtst_t[:, i] = le_x.transform(Xtst[:, i])
    le_y = preprocessing.LabelEncoder()
    le_y.fit(['negative', 'positive'])
    ytrn_t = le_y.transform(ytrn)
    ytst_t = le_y.transform(ytst)
    for d in [1,3,5]:
        decision_tree = id3(Xtrn, ytrn_t, max_depth=d)
        dot_str = to_graphviz(decision_tree)
        render_dot_file(dot_str, './question_E_depth=' + str(d))
        y_pred = [predict_example(x, decision_tree) for x in Xtst]
        print("Confusion Matrix for depth = " + str(d))
        print(confusion_matrix(ytst_t, y_pred))
        model = DecisionTreeClassifier(criterion='entropy', max_depth=d)
        model.fit(Xtrn_t, ytrn_t)
        y_pred = model.predict(Xtst_t)
        print("Confusion Matrix for depth = " + str(d) + " (sklearn model)")
        print(confusion_matrix(ytst_t, y_pred))
        

