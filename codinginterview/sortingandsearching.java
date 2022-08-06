import java.util.Arrays;

class SortingAndSearching {
    static void mergesort(int[] array) {
        int[] helper = new int[array.length];
        mergesort(array, helper, 0, array.length-1);
    }

    static void mergesort(int[] array, int[] helper, int low, int high) {
        if (low < high) {
            int middle = (low + high) / 2;
            mergesort(array, helper, low, middle);  // Sort left half
            mergesort(array, helper, middle+1, high);   // Sort right half
            merge(array, helper, low, middle, high);    // Merge them
        }
    }

    static void merge(int[] array, int[] helper, int low, int middle, int high) {
        /* Copy both halves into a helper array */
        for (int i = low; i <= high; i++) {
            helper[i] = array[i];
        }
        int helperLeft = low;
        int helperRight = middle + 1;
        int current = low;

        /*
         * Iterate though helper array. Compare the left and right half, copying back
         * the smaller element from the two halves into the original array
         */
        while (helperLeft <= middle && helperRight <= high) {
            if (helper[helperLeft] <= helper[helperRight]) {
                array[current] = helper[helperLeft];
                helperLeft++;
            } else {
                array[current] = helper[helperRight];
                helperRight++;
            }
            current++;
        }

        /* Copy the rest of the left side of the array into the target array */
        int remaining = middle - helperLeft;
        for (int i = 0; i <= remaining; i++) {
            array[current+i] = helper[helperLeft+i];
        }
    }

    static void quickSort(int[] arr) {
        quickSort(arr, 0, arr.length-1);
    }

    static void quickSort(int[] arr, int left, int right) {
        int index = partition(arr, left, right);
        if (left < index-1) {
            // Sort left half
            quickSort(arr, left, index-1);
        }
        if (index < right) {
            // Sort right half
            quickSort(arr, index, right);
        }
    }

    static int partition(int[] arr, int left, int right) {
        int pivot = arr[(left+right)/2];    // Pick pivot point
        while (left <= right) {
            // Find element on left that should be on right
            while (arr[left] < pivot) left++;

            // Find element on right that should be on left
            while (arr[right] > pivot) right--;

            // Swap elements on right that should be on left
            if (left <= right) {
                int t = arr[left];
                arr[left] = arr[right];
                arr[right] = t;
                left++;
                right--;
            }
        }
        return left;
    }

    static int binarySearch(int[] a, int x) {
        int low = 0;
        int high = a.length-1;
        int mid;

        while (low <= high) {
            mid = low + (high-low) / 2;
            assert mid == (low+high) / 2;
            if (a[mid] < x) low = mid + 1;
            else if (a[mid] > x) high = mid - 1;
            else return mid;
        }
        return -1;
    }

    static int binarySearchRecursive(int[] a, int x) {
        return binarySearchRecursive(a, x, 0, a.length-1);
    }

    static int binarySearchRecursive(int[] a, int x, int low, int high) {
        if (low > high) return -1;

        int mid = (low + high) / 2;
        if (a[mid] < x) return binarySearchRecursive(a, x, mid+1, high);
        else if (a[mid] > x) return binarySearchRecursive(a, x, low, mid-1);
        else return mid;
    }

    public static void main(String[] args) {
        int[] array = new int[] { 4, 7, 1, 6, 9, 2, 0 };
        quickSort(array);
        System.out.println(Arrays.toString(array));
        System.out.println(binarySearchRecursive(array, 2));
    }
}