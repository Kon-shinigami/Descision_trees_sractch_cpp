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
}; 

bool isnum (string datapart){
     if( datapart.size() == 0 ){
      return false ; 
     }
     for (int i = 0 ; i < datapart.size() ; i++ ){
     char st = datapart[i] ; 
     if ( isdigit(st) ==  0 && st != '-' && st != '.') {
        return false ; 
     }
     }
     return true  ; 
}
 vector<vector<float>> data() {
   unordered_map<string,float>hash   ; 
   string line  ;
   vector<vector<float>>data ; 
   float code  = 0.0 ; 
   ifstream file("data.csv");
    while (getline(file, line)) {
     stringstream ss(line);
     string datapart;
      vector<float> row ;
      bool skip = false;  
     while (getline(ss, datapart, ',')) {
        if (datapart.size() == 0 ){
            skip = true ; 
            break ; 
        }
        if (isnum(datapart) == true){
            row.push_back(stof(datapart)) ;
        }
        else {
            if (hash.count(datapart)  == 0 ){
                code = code + 1.0 ; 
                hash[datapart] = code  ; 
                row.push_back(code) ; 
            }
            else {
                row.push_back(hash[datapart])  ; 
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
    for(int i = 0 ; i < n ; i++){
     gini = gini + pow(counts[i]/sum , 2) ; 
    }
    gini = 1 - gini ; 
    return gini ;
}
float gini_impurity_tree(vector<float> &right_subtree , vector<float>& left_subtree){
    int n = right_subtree.size() ; 
    float r ; 
    for (int i = 0 ; i< n ; i++  ){
    r = r + right_subtree[i] ; 
    }
    float l ; 
    int m = left_subtree.size() ;
    for (int i = 0 ; i< m ; i++ ){
        l = l + left_subtree[i] ; 
    }
    float tot = r+l ; 
    float gini_final = (r/tot)*(gini_impurity_subtree(right_subtree)) + (l/tot)*(gini_impurity_subtree(left_subtree) ) ; 
    return gini_final ; 
}

vector<float>get_perfect(vector<pair<float , float>> pr , vector<pair<float , int>>pos , vector<float>target_things ){
    vector<float>smallest_gini_imp ; 
    unordered_map<float , float > hash_left ;
    unordered_map<float , float > hash_right ; 
    vector<float>left ; 
    vector<float>right ; 
    float num ; 
    float gini = 100 ; 
    float gini_imp ; 
    int position  = 0  ; 
    float gini_end ; 

    int m = pr.size() ; 
    for (int i = 0 ; i< pos.size() ; i++ ){
     hash_left.clear();
     hash_right.clear();
     left.clear();
     right.clear();
     int g = pos[i].second ; 
     for (int j =  0 ; j < g ;j++){
     hash_left[pr[j].second] = hash_left[pr[j].second] + 1.0 ; 
     }
     for (int j =  g ; j < m ;j++){
     hash_right[pr[j].second] = hash_right[pr[j].second] + 1.0 ; 
     }
     int n = target_things.size() ; 
     for (int i = 0 ; i< n ;i++){
        left.push_back(hash_left[target_things[i]]) ; 
        right.push_back(hash_right[target_things[i]]) ; 
     }
     gini_imp = gini_impurity_tree(right  , left) ; 
     gini = min(gini ,gini_imp ) ; 
     if (gini > gini_imp) {
      num = pos[i].first  ; 
      position = pos[i].second ; 
      gini_end = gini_imp ; 
     }
    }
        smallest_gini_imp.push_back(num)  ; 
        smallest_gini_imp.push_back(position)  ; 
        smallest_gini_imp.push_back(gini_end)  ; 
  return smallest_gini_imp ; 

}

vector<float> perfect_variable(vector<vector<float>>data){
    int n = data.size() ; 
    vector<float>target = data[n-1]  ; 
    float a  ; 
    float b ; 
    vector<float>final_ans ; 
    vector<float >target_things  ; 
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
     float gini ; 
     
        float gini_impurity  = 100 ; 
        float num ; 
    for ( int i = 0 ; i< n-1 ; i++){
        vector<pair<float  , float>> pr ; 
        for (int j = 0  ; j < data[i].size() ; j++ ){
            pr.push_back({data[i][j] , target[j]}) ; 
        }
        sort(pr.begin() , pr.end()) ; 
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
            a = (float)i ;
            b = vec[1] ; 
        }
    }
 final_ans.push_back(gini_impurity  ) ; 
 final_ans.push_back( num ) ; 
 final_ans.push_back( a ) ; 
 final_ans.push_back(b ) ;
 
}

vector<vector<vector<float>>> new_dataset( vector<vector<float>> data) {
    vector<float> prev_data = perfect_variable(data) ; 
    int on_basis = (int)prev_data[2];
    int thresh = (int)prev_data[3];
    vector<vector<vector<float>>> result;
    sort(data.begin(), data.end(),
        [on_basis](vector<float>& a, vector<float>& b) {
            return a[on_basis] < b[on_basis];
        });

    vector<vector<float>> left_data(data.begin(), data.begin() + thresh + 1);
    for (int i = 0; i < left_data.size(); i++) {
        left_data[i].erase(left_data[i].begin() + on_basis);
    }
    vector<vector<float>> right_data(data.begin() + thresh + 1, data.end());
    for (int i = 0; i < right_data.size(); i++) {
        right_data[i].erase(right_data[i].begin() + on_basis);
    }
    result.push_back(left_data);
    result.push_back(right_data);
    return result;
}



void rec(vector<vector<float>>dta , int i  , int n  , TreeNode*tree){
    if (i > 9 || n ==0 ){
        return  ; 
    }
    vector<vector<vector<float>>>dt  = new_dataset(dta);
    vector<vector<float>>a = dt[0] ; 
    vector<vector<float>>b = dt[1] ; 
    




}

void main(){
    TreeNode* tree ; 
    float gini_threshold ; 
    int max_height ; 
    int i  = 0 ; 
    vector<vector<float>>data_set = data() ; 
    int size = data_set[0].size() ; 
    vector<float>items = perfect_variable(data_set) ; 
    

    }


    
}



