#include <windows.h>
#include <vector>
#include <string>
#include <fstream>

enum SortState {
    Constructed,
    DataLoaded,
    Processing,
    Done
};

class SortingJob {
public:
    SortingJob() : state(Constructed), i(0), j(0) {}
    ~SortingJob() {
        data.clear();
    }

    void DoSomeProcessing() {
        switch (state) {
            case Constructed:
                LoadData();
                state = DataLoaded;
                break;
            case DataLoaded:
                i = 0;
                j = 0;
                state = Processing;
                break;
            case Processing:
                ProcessSorting();
                break;
            case Done:
                break;
        }
    }

    bool IsDone() const {
        return state == Done;
    }

    std::vector<std::string> data;
    SortState state;

private:
    void LoadData() {
        std::ifstream file("c:\\myhugedatafile.txt");
        std::string line;
        while (std::getline(file, line)) {
            data.push_back(line);
        }
    }

    void ProcessSorting() {
        int iterations = 0;
        bool swapped;
        do {
            swapped = false;
            for (; i < data.size() - 1; ++i) {
                for (; j < data.size() - i - 1; ++j) {
                    if (data[j] > data[j + 1]) {
                        std::swap(data[j], data[j + 1]);
                        swapped = true;
                    }
                    if (++iterations >= 10) {
                        return;
                    }
                }
                j = 0;
            }
            i = 0;
        } while (swapped);
        state = Done;
    }

    size_t i, j;
};

SortingJob* sortingJob = nullptr;

LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam) {
    switch (uMsg) {
        case WM_COMMAND:
            if (LOWORD(wParam) == ID_SORT_BUTTON) {
                EnableWindow(GetDlgItem(hwnd, ID_SORT_BUTTON), FALSE);
                if (sortingJob) delete sortingJob;
                sortingJob = new SortingJob();
                SetTimer(hwnd, 1, 1000, NULL);
            }
            break;
        case WM_TIMER:
            if (sortingJob) {
                sortingJob->DoSomeProcessing();
                if (sortingJob->IsDone()) {
                    KillTimer(hwnd, 1);
                    MessageBox(hwnd, "Bubble sort is done!", "Info", MB_OK);
                    EnableWindow(GetDlgItem(hwnd, ID_SORT_BUTTON), TRUE);
                    delete sortingJob;
                    sortingJob = nullptr;
                }
            }
            break;
        case WM_DESTROY:
            if (sortingJob) delete sortingJob;
            PostQuitMessage(0);
            break;
        default:
            return DefWindowProc(hwnd, uMsg, wParam, lParam);
    }
    return 0;
}
