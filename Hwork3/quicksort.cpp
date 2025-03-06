#include <iostream>
#include <stack>
using namespace std;

// Function to partition the array
int partition(int arr[], int low, int high) {
    int pivot = arr[high];  // Choosing the last element as pivot
    int i = low - 1;  // Index of smaller element

    for (int j = low; j < high; j++) {
        if (arr[j] <= pivot) {  // If current element is smaller than or equal to pivot
            i++;
            swap(arr[i], arr[j]);
        }
    }
    swap(arr[i + 1], arr[high]);
    return (i + 1);  // Return partition index
}

// Function to implement non-recursive Quicksort
void quickSortIterative(int arr[], int low, int high) {
    stack<int> stk;

    // Push initial values of low and high to the stack
    stk.push(low);
    stk.push(high);

    while (!stk.empty()) {
        // Pop high and low
        high = stk.top(); stk.pop();
        low = stk.top(); stk.pop();

        // Partition the array
        int p = partition(arr, low, high);

        // If there are elements on the left of pivot, push left subarray
        if (p - 1 > low) {
            stk.push(low);
            stk.push(p - 1);
        }

        // If there are elements on the right of pivot, push right subarray
        if (p + 1 < high) {
            stk.push(p + 1);
            stk.push(high);
        }
    }
}

// Function to print an array
void printArray(int arr[], int size) {
    for (int i = 0; i < size; i++)
        cout << arr[i] << " ";
    cout << endl;
}

// Main function to test the Quicksort implementation
int main() {
    int arr[] = {10, 7, 8, 9, 1, 5};
    int n = sizeof(arr) / sizeof(arr[0]);

    cout << "Original array: ";
    printArray(arr, n);

    quickSortIterative(arr, 0, n - 1);

    cout << "Sorted array: ";
    printArray(arr, n);

    return 0;
}
