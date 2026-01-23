#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <unordered_map>
#include <utility>
#include<algorithm>
using namespace std;



struct TreeNode {
  TreeNode* left;
    TreeNode* right;
    float value;
    float* leaf_status_left ;
    float*leaf_status_right ;  
}; 

unordered_map<float,int>used_variables ; 
bool isnum (string datapart){
     if( datapart.size() == 0 ){
      return false ; 
     }
     for (int i = 0 ; i < datapart.size() ; i++ ){
     char st = datapart[i] ; 
     if (i == 0 ){
     if ( isdigit(st) ==  0 && st != '-' && st != '.') {
        return false ; 
     }
    }
     if ( isdigit(st) ==  0 && st != '.' ) {
        return false ; 
     }
     }
     return true  ; 
}


 vector<vector<float>> data() {
   vector<unordered_map<string,float>>hash   ; 
    
   string line  ;
   vector<vector<float>>data ; 
   vector<float> code  ; 
   ifstream file("data.csv");
    bool first = false ; 
    while (getline(file, line)) {
     stringstream ss(line);
     string datapart;
      vector<float> row ;
      bool skip = false;  
      int i = 0 ; 
     while (getline(ss, datapart, ',')) {
        if (first == false){
             hash.push_back(unordered_map<string,float>()); 
            code.push_back(0.0);
        }
        if (datapart.size() == 0 ){
            skip = true ; 
            break ; 
        }
        if (isnum(datapart) == true){
            row.push_back(stof(datapart)) ;
        }
        else {
            if (hash[i].count(datapart)  == 0 ){
                code[i] = code[i] + 1.0 ; 
                hash[i][datapart] = code[i]  ; 
                row.push_back(code[i]) ; 
            }
            else {
                row.push_back(hash[i][datapart])  ; 
            }

        }
    }
    if (skip == false ){
        data.push_back(row) ; 
    }
} 
return data ; 
}


float gini_impurity_subtree(vector<float>&counts ){
    int n = counts.size() ; 
    float sum = 0.0 ; 
    float gini = 0.0  ; 
    for(int i  = 0 ; i< n ; i++){
     sum = sum + counts[i] ; 
    }
    if (sum == 0.0){
        return 0.0 ; 
    }
    for(int i = 0 ; i < n ; i++){
     gini = gini + pow(counts[i]/sum , 2) ; 
    }
    gini = 1 - gini ; 
    return gini ;
}


vector<float> gini_impurity_tree(vector<float> &right_subtree , vector<float>& left_subtree){
    int n = right_subtree.size() ; 
    vector<float>ans ; 
    float max_right  = 0.0 ; 
    float r = 0.0 ; 
     int a  = 0 ; 
    int b = 0 ; 
    for (int i = 0 ; i< n ; i++  ){
    r = r + right_subtree[i] ; 
    max_right = max(max_right ,right_subtree[i] ) ;
    if (max_right == right_subtree[i]){
           a = i  ; 
    }
    }
    float l = 0.0 ; 
    int m = left_subtree.size() ;
    float max_left = 0.0 ;
    for (int j = 0 ; j< m ; j++ ){
        l = l + left_subtree[j] ; 
        max_left = max(max_left ,left_subtree[j] ) ;
        if (max_left == left_subtree[j]){
           b = j  ; 
        }
    }
    float tot = r+l ; 
    if (tot == 0.0) {             
        ans.push_back(0.0);
        ans.push_back(0.0);
        ans.push_back(0.0);
        ans.push_back(0.0) ;
        ans.push_back(0.0) ;  
        
        return ans;
    }
    float right = gini_impurity_subtree(right_subtree) ; 
    float left = gini_impurity_subtree(left_subtree) ; 
    float gini_final = (r/tot)*(right) + (l/tot)*(left ) ; 
    ans.push_back(gini_final) ; 
    ans.push_back(left) ;
    ans.push_back(right) ; 
    ans.push_back((float)a) ; 
    ans.push_back((float)b) ;  

    return ans ; 
}


vector<float>get_perfect(vector<pair<float , float>> pr , vector<pair<float , int>>pos , vector<float>target_things){

    vector<float>smallest_gini_imp ; 
    int m = pr.size() ; 
    int n = target_things.size() ; 
    vector<float > hash_left(n,0) ;
    vector< float > hash_right(n,0) ; 
    float num = 0.0  ; 
    float gini = 100 ; 
    vector<float> gini_imp ; 
    int position  = 0  ; 
    float gini_end = 0.0  ; 
    float gini_cmp ; 
    float gini_left  = 0.0 ;  
    float gini_right = 0.0;
    float left_leaf = 0.0 ; 
    float right_leaf = 0.0 ; 


    if (pos.size() == 0) {
        smallest_gini_imp.push_back(0.0);
        smallest_gini_imp.push_back(0);
        smallest_gini_imp.push_back(100.0);
        smallest_gini_imp.push_back(0.0);
        smallest_gini_imp.push_back(0.0);
        return smallest_gini_imp;
    }


    for (int i = 0 ; i < m ; i++ ){
     hash_right[pr[i].second] = hash_right[pr[i].second] + 1.0 ; 
    }


    for (int i = 0 ; i< pos.size() ; i++ ){
     int g = pos[i].second ; 
     hash_left[pr[g].second] = hash_left[pr[g].second] + 1.0 ; 
     hash_right[pr[g].second] = hash_right[pr[g].second] - 1.0 ; 
     gini_imp = gini_impurity_tree(hash_right  , hash_left) ; 
     gini_cmp = gini_imp[0] ; 

     if (gini > gini_cmp) {
      gini = gini_cmp ;
      num = pos[i].first  ; 
      position = pos[i].second ; 
      gini_end = gini_cmp ; 
      gini_left = gini_imp[1] ; 
      gini_right = gini_imp[2] ; 
      left_leaf = target_things[(int)gini_imp[4]] ; 
      right_leaf = target_things[(int)gini_imp[3]] ; 
     }
    }
        smallest_gini_imp.push_back(num)  ; 
        smallest_gini_imp.push_back(position)  ; 
        smallest_gini_imp.push_back(gini_end)  ; 
        smallest_gini_imp.push_back(gini_left) ; 
        smallest_gini_imp.push_back(gini_right) ; 
        smallest_gini_imp.push_back(left_leaf) ; 
        smallest_gini_imp.push_back(right_leaf) ; 
  return smallest_gini_imp ; 

}


unordered_map<float , pair<float , float>>leaf_info ; 


pair<vector<vector<pair<float,pair<float,float>>>>, vector<vector<pair<float,pair<float,float>>>>>new_dataset( vector<float>prev_data  ,vector<vector<pair<float , pair<float,float>>>>  data) {
    int on_basis = (int)prev_data[2];
    int thresh = (int)prev_data[3];
    int m = data[0].size() ; 
    unordered_map<float , int>left_index ; 
    unordered_map<float , int >right_index ; 
    for (int i = 0 ; i < thresh ; i++ ){
        left_index[data[on_basis][i].second.first]++ ; 
    }
    for (int i = thresh+1 ; i<  m ; i++){
        right_index[data[on_basis][i].second.first]++ ; 
    }
    vector<vector<pair<float,pair<float,float>>>>left_data ; 
    vector<vector<pair<float,pair<float,float>>>>right_data ; 
    for (int i = 0 ; i < data.size() ; i++ ){
        for (int j = 0 ; j < data[i].size() ; j++ ){
            if (used_variables[i] > 0 ){
            pair<float,pair<float,float>>vec = {data[i][j].first , {data[i][j].second.first ,data[i][j].second.second}} ; 
            if (left_index[data[i][j].second.first] > 0 ){
                left_data[i].push_back(vec) ; 
            }
            else {
                 right_data[i].push_back(vec) ; 
            }
        }
    }
    } 
    return  {left_data,right_data} ; 
}



void rec(vector<vector<pair<float , pair<float,float>>>>  dta , int i  , int n  , TreeNode*tree){
    if (i > 9 || n == 0 ){
        return  ; 
    }
    vector<float>thing = perfect_variable(dta) ; 
    tree->value = thing[1] ; 
    used_variables[thing[2]]++ ; 
    pair<vector<vector<pair<float,pair<float,float>>>>, vector<vector<pair<float,pair<float,float>>>>>data_sm  = new_dataset(thing  , dta);
    vector<vector<pair<float , pair<float,float>>>> left_part = data_sm.first  ; 
    vector<vector<pair<float , pair<float,float>>>> right_part = data_sm.second ; 
    if (thing[4] > 0.01){
    tree->left = new TreeNode();
    rec(left_part , i+1 , n-1 ,  tree->left) ;
    }
    if (thing[5] > 0.01){ 
     tree->right = new TreeNode();
    rec(right_part , i+1 , n-1 , tree->right) ; 
    }
    return  ; 
}


int main(){
    TreeNode* tree = new TreeNode() ; 
    int i  = 0 ; 
    vector<vector<float>>data_set = data() ; 
    vector<vector<pair<float , pair<float,float>>>>  data_new = sorted_dataset(data_set) ; 
    int size = data_new[0].size() ; 
    rec(data_new , i , size , tree ) ; 
    leaf(tree , leaf_info) ; 
    return 0 ; 
}

void leaf(TreeNode*tree  , unordered_map<float , pair<float , float>>leaf_info ){
    if (tree == nullptr  ){
        return ; 
    }
    if (tree->leaf_status_left != nullptr && tree->leaf_status_right != nullptr){
        return ; 
    }
    if (tree->left == nullptr && tree->leaf_status_left == nullptr ){
      tree->left = new TreeNode() ; 
      float val = leaf_info[tree->value].first ; 
      tree->left->value = val ; 
     tree->leaf_status_left = new float(1.0);
    }
    else if  (tree->right ==  nullptr && tree->leaf_status_right == nullptr){
     tree->right = new TreeNode() ; 
      float val = leaf_info[tree->value].second ; 
      tree->right->value = val ;  
      tree->leaf_status_right = new float(1.0);
    }
    if (tree == nullptr && tree->leaf_status_left != nullptr && tree->leaf_status_right != nullptr ){
        return ; 
    }
     leaf(tree->left , leaf_info) ; 
     leaf(tree->right , leaf_info) ; 
}



vector<vector<pair<float , pair<float,float>>>> sorted_dataset(vector<vector<float>>data  ){
    int n = data.size() ; 
    vector<vector<pair<float , pair<float,float>>>>data_info(n-1) ;
    int m = data[0].size() ; 
    float num ;
    float index ; 
    float target ; 
    pair<float,float>temp ; 
    pair<float,pair<float,float>>temp_data ; 

    for (int i = 0 ;  i< n ; i++){
        for (int j = 0 ; j < m ; j++ ){
            num = data[i][j] ; 
            target = data[n-1][j] ; 
            temp = {index , target} ; 
            index = (float)j ; 
            temp_data = {num,temp} ; 
            data_info[i].push_back(temp_data) ; 
        }
        sort(data_info[i].begin() , data_info[i].end()) ;
    }
    return data_info ; 
}

vector<float>target_things ; 
void unique_target(vector<vector<float>>data){
int n = data.size() ; 
vector<float>target = data[n-1]  ; 
unordered_map<float , int > tt ; 
for (int i = 0 ; i < target.size() ; i++){
    
        if (tt[target[i]] > 0 ){
            continue ; 
        }
        else {
          target_things.push_back(target[i]) ; 
          tt[target[i]]++ ; 
        }
    }
}


vector<float> perfect_variable(vector<vector<pair<float , pair<float,float>>>> data_new){
    int n = data_new.size() ; 
    vector<float>row  ; 
    float variable_pos  ; 
    float left_leaf = 0.0 ; 
    float right_leaf = 0.0 ; 
    float thresh_data_loc ; 
    vector<float>final_ans ; 
    float gini_left ; 
    float gini_right ; 
     float gini ; 
     float gini_impurity  = 100 ; 
     float num ; 

    for ( int i = 0 ; i< n-1 ; i++){
        vector<pair<float  , float>> pr ; 
        for (int j = 0  ; j < data_new[i].size() ; j++ ){
            pr.push_back({data_new[i][j].first , data_new[i][j].second.second}) ; 
        }
        vector<pair<float  , int>>pos ; 
        for (int k = 0 ; k< pr.size()-1 ; k++){
         float   first = pr[k].first ; 
         float second  = pr[k+1].first ; 
         float mid  = ( first + second)/2 ; 
         pos.push_back({mid , k+1 }) ; 
        }
        vector<float>vec = get_perfect(pr,pos,target_things ) ; 
       gini = vec[2] ;  
        if (gini < gini_impurity){
            gini_impurity = gini ; 
            num = vec[0] ; 
            variable_pos = (float)i ;
            thresh_data_loc = vec[1] ; 
            gini_left = vec[3] ; 
            gini_right = vec[4] ; 
            left_leaf = vec[5] ; 
            right_leaf = vec[6] ; 
        }
    }

 final_ans.push_back(gini_impurity  ) ; 
 final_ans.push_back( num ) ; 
 final_ans.push_back( variable_pos ) ; 
 final_ans.push_back(thresh_data_loc ) ;
 final_ans.push_back( gini_left ) ; 
 final_ans.push_back(gini_right ) ;
 leaf_info[num] = {left_leaf, right_leaf};
 return final_ans ; 
}
