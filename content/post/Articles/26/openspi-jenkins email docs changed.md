---
title: OpenAPI-Use Jenkins to Detect OpenAPI Changes + Email Docs Team
description: ""
slug: detect-openapi-changes-jenkins
date: 2019-07-23
image: post/Articles/IMAGES/openapi.png
categories:
  - API Documentation
  - CI/CD
  - Jenkins
  - OpenAPI
  - Web Development
  - CI\CD
  - Cloud
  - DevOps
tags:
  - Jenkins
  - OpenAPI
  - Swagger
  - API
  - Documentation
  - Automation
draft: false
weight: 552
lastmod: 2025-02-26T11:19:31.993Z
---
So, you’re maintaining **manual API documentation**, but how do you know **when** to update it?

If your API changes, but nobody tells the documentation team... **you’ve got a problem.**

What if Jenkins could:\
✅ **Detect when swagger.json changes**\
✅ **Log the differences in a file**\
✅ **Trigger a documentation update notification**\
✅ **Fail the build if documentation updates are ignored**

***

## 🔥 The Plan

We’re setting up **two Jenkins jobs** to monitor OpenAPI changes:

1️⃣ **API Repo Jenkins Job** – Detects changes in `swagger.json` and logs them in a `apichangessensedbyjenkins.txt` file.\
2️⃣ **Docs Repo Jenkins Job** – Watches for changes in this file and emails the doc team with the API diff.

If the diff logging **fails**, the **API build fails**.\
If documentation updates are **ignored**, a **reminder email is sent**.

***

## 🛠 Step 1: Set Up Jenkins to Detect OpenAPI Changes

### ✅ 1.1: Install Required Tools on Jenkins Server

Make sure Jenkins has:

* **Git** (`apt install git` or `choco install git`)
* **jq** (JSON diff tool, `apt install jq` or `choco install jq`)
* **diff** (should be pre-installed)

***

### ✅ 1.2: Create Jenkins Job to Detect API Changes

1. Open **Jenkins Dashboard** → **New Item** → **Freestyle Project**
2. Name it **Detect-OpenAPI-Changes**
3. Under **Source Code Management**, choose **Git**, and add your API repo URL.
4. Under **Build Triggers**, enable **Poll SCM** (e.g., every 5 minutes: `H/5 * * * *`).
5. Add the following **Bash Script** under **Build Steps → Execute Shell**:

```sh
#!/bin/bash

# Define paths
API_REPO="/var/lib/jenkins/workspace/Detect-OpenAPI-Changes"
DOCS_REPO="/var/lib/jenkins/workspace/API-Docs-Changes"
SWAGGER_FILE="$API_REPO/swagger.json"
OLD_SWAGGER_FILE="$API_REPO/old_swagger.json"
CHANGE_LOG="$DOCS_REPO/apichangessensedbyjenkins.txt"

# Clone docs repo
if [ ! -d "$DOCS_REPO" ]; then
    git clone git@github.com:your-org/api-docs.git "$DOCS_REPO"
fi

# Pull latest changes
cd "$DOCS_REPO"
git pull origin main

# Check if old swagger file exists
if [ -f "$OLD_SWAGGER_FILE" ]; then
    # Compute JSON diff
    DIFF=$(diff <(jq -S . "$OLD_SWAGGER_FILE") <(jq -S . "$SWAGGER_FILE"))

    if [ "$DIFF" != "" ]; then
        echo "Swagger JSON has changed!"
        
        # Append diff to log file
        echo "----------------------------" >> "$CHANGE_LOG"
        echo "Date: $(date)" >> "$CHANGE_LOG"
        echo "Commit: $(git -C "$API_REPO" rev-parse HEAD)" >> "$CHANGE_LOG"
        echo "Changes:" >> "$CHANGE_LOG"
        echo "$DIFF" >> "$CHANGE_LOG"
        
        # Push changes to docs repo
        git add "$CHANGE_LOG"
        git commit -m "API changes detected by Jenkins on $(date)"
        git push origin main
    else
        echo "No changes detected in Swagger JSON."
    fi
else
    echo "First run, saving initial Swagger file."
fi

# Save new Swagger file for future comparison
cp "$SWAGGER_FILE" "$OLD_SWAGGER_FILE"
```

6. **Save and Build Now**.
7. Check if `apichangessensedbyjenkins.txt` is created/updated in your **API-Docs** repo.

***

## 📩 Step 2: Set Up Jenkins to Notify the Documentation Team

### ✅ 2.1: Create a Second Jenkins Job

1. Open **Jenkins Dashboard** → **New Item** → **Freestyle Project**
2. Name it **Notify-Docs-Team**
3. Under **Source Code Management**, choose **Git**, and add the **API-Docs repo URL**.
4. Under **Build Triggers**, enable **Poll SCM** (`H/5 * * * *`).
5. Under **Build Steps → Execute Shell**, add:

```sh
#!/bin/bash

DOCS_REPO="/var/lib/jenkins/workspace/API-Docs-Changes"
CHANGE_LOG="$DOCS_REPO/apichangessensedbyjenkins.txt"

# Pull latest changes
cd "$DOCS_REPO"
git pull origin main

# Check if file has been updated
if git diff --name-only HEAD~1 HEAD | grep -q "apichangessensedbyjenkins.txt"; then
    echo "Swagger changes detected! Sending email notification..."
    
    # Extract last changes
    DIFF_CONTENT=$(tail -n 20 "$CHANGE_LOG")

    # Send email
    echo -e "Subject: API Documentation Update Required\n\nThe following API changes were detected:\n\n$DIFF_CONTENT" | sendmail -v docs-team@example.com
else
    echo "No new API changes detected."
fi
```

6. **Save and Build Now**.
7. Check email inbox for **API change notifications**.

***

## 🚨 Step 3: Fail Build if API Docs Aren't Updated

If the documentation team **doesn’t update** the API docs (by updating the swagger.json) after changes, we can **fail the build**.

1. In **Notify-Docs-Team**, go to **Post-Build Actions → Add Post-Build Script**
2. Add the following check:

```sh
#!/bin/bash

DOCS_REPO="/var/lib/jenkins/workspace/API-Docs-Changes"
CHANGE_LOG="$DOCS_REPO/apichangessensedbyjenkins.txt"

# Get last commit message
LAST_COMMIT_MSG=$(git -C "$DOCS_REPO" log -1 --pretty=%B)

# If the last commit was by Jenkins, the docs haven't been updated yet
if [[ "$LAST_COMMIT_MSG" == *"API changes detected by Jenkins"* ]]; then
    echo "Documentation has not been updated. Failing build."
    exit 1
else
    echo "Documentation updated. Build successful."
fi
```

Now, if the docs team **doesn’t update API documentation**, the **build will fail**, forcing them to take action.

***

<!-- 
## 🎯 Conclusion: Automating API Documentation Updates

✅ **Jenkins now automatically detects OpenAPI changes**  
✅ **It logs differences in `apichangessensedbyjenkins.txt`**  
✅ **It triggers email notifications to the documentation team**  
✅ **It fails the build if documentation updates are ignored**  

With this setup, **no API change goes unnoticed**, and documentation stays **up-to-date**. 🚀

Now go forth and **automate your API docs like a boss!** 🏆

---

## 🔑 Key Takeaways

| Summary        | Details |
|---------------|---------|
| **How to detect API changes?** | Compare `swagger.json` with an old version. |
| **Where to store API diffs?** | Log them in `apichangessensedbyjenkins.txt` in another repo. |
| **How to notify documentation teams?** | Send email with API diffs when the file changes. |
| **How to enforce updates?** | Fail the build if documentation isn't updated. |
| **Why use Jenkins?** | Automates API change tracking without manual effort. |

```
-->

So, Jenkins is now **detecting API changes**, logging diffs, and **failing the build** if documentation isn’t updated. **Great!**

But now the docs team has **a new problem**:\
➡️ How do they actually **update the Swagger JSON** and prevent build failures?

The answer? **A simple Python script.**\
(Which is the answer to **most things in life** hahaha.. )
----------------------------------------------------------

# How The Docs Team can push a button after the docs are fix- and make the build work again

## 🚀 The Plan

The docs team will:\
✅ Run a Python script to pull the latest `swagger.json`\
✅ Manually review and update the documentation\
✅ Commit and push the updated `swagger.json`\
✅ Fix the **Jenkins failure**, keeping API docs up to date

Let’s build it. 🚀

***

## 🔧 Step 1: Install Dependencies

The script uses **GitPython** to manage Git operations and **jsondiff** to detect changes in `swagger.json`.

### Install them with:

```sh
pip install gitpython jsondiff
```

***

## 📝 Step 2: Create the Python Script

Save the following script as `update_swagger.py` in the **manual documentation repository**.

```python
import os
import json
import git
import subprocess
from jsondiff import diff

# Set repository paths
DOCS_REPO_PATH = os.path.abspath(".")  # Run this script inside the docs repo
API_REPO_URL = "git@github.com:your-org/api-repo.git"  # API repository
TEMP_CLONE_PATH = "/tmp/api-repo"  # Temporary API clone location
SWAGGER_FILE = "swagger.json"
CHANGE_LOG_FILE = "apichangessensedbyjenkins.txt"

def clone_api_repo():
    """Clone the API repository to fetch the latest swagger.json."""
    if os.path.exists(TEMP_CLONE_PATH):
        subprocess.run(["rm", "-rf", TEMP_CLONE_PATH], check=True)
    
    print("Cloning API repository...")
    git.Repo.clone_from(API_REPO_URL, TEMP_CLONE_PATH)
    
def fetch_swagger_file():
    """Fetch the latest swagger.json from the API repo."""
    api_swagger_path = os.path.join(TEMP_CLONE_PATH, SWAGGER_FILE)
    
    if not os.path.exists(api_swagger_path):
        raise FileNotFoundError("Swagger file not found in API repository!")
    
    with open(api_swagger_path, "r") as f:
        return json.load(f)

def load_current_swagger():
    """Load the existing swagger.json from the docs repo."""
    docs_swagger_path = os.path.join(DOCS_REPO_PATH, SWAGGER_FILE)
    
    if not os.path.exists(docs_swagger_path):
        print("No existing swagger.json found. Creating a new one...")
        return {}
    
    with open(docs_swagger_path, "r") as f:
        return json.load(f)

def detect_changes(old, new):
    """Detect changes between old and new Swagger JSON."""
    return diff(old, new, syntax="symmetric")

def update_swagger_file(new_swagger):
    """Update the local swagger.json file in the documentation repo."""
    docs_swagger_path = os.path.join(DOCS_REPO_PATH, SWAGGER_FILE)
    
    with open(docs_swagger_path, "w") as f:
        json.dump(new_swagger, f, indent=2)
    
    print(f"Updated {SWAGGER_FILE} with the latest API changes.")

def append_to_changelog(changes):
    """Append detected changes to the Jenkins change log file."""
    changelog_path = os.path.join(DOCS_REPO_PATH, CHANGE_LOG_FILE)
    
    with open(changelog_path, "a") as f:
        f.write("\n----------------------------\n")
        f.write(f"Date: {subprocess.getoutput('date')}\n")
        f.write(f"Changes detected:\n{json.dumps(changes, indent=2)}\n")
    
    print(f"Changes appended to {CHANGE_LOG_FILE}.")

def commit_and_push():
    """Commit and push the updated swagger.json to prevent Jenkins build failure."""
    repo = git.Repo(DOCS_REPO_PATH)
    
    repo.git.add(SWAGGER_FILE)
    repo.git.add(CHANGE_LOG_FILE)
    
    commit_message = "Updated Swagger JSON to match API changes."
    repo.git.commit("-m", commit_message)
    
    print("Pushing changes to documentation repository...")
    repo.git.push("origin", "main")
    
    print("Swagger JSON updated and pushed successfully!")

if __name__ == "__main__":
    try:
        clone_api_repo()
        
        new_swagger = fetch_swagger_file()
        old_swagger = load_current_swagger()
        
        changes = detect_changes(old_swagger, new_swagger)
        
        if changes:
            print("Changes detected! Updating documentation...")
            update_swagger_file(new_swagger)
            append_to_changelog(changes)
            commit_and_push()
            print("✅ Documentation successfully updated.")
        else:
            print("No changes detected. Documentation is already up to date.")
    
    except Exception as e:
        print(f"❌ Error: {e}")
        exit(1)
```

***

## 📌 Step 3: Run the Script

The documentation team can now update the API docs with:

```sh
python update_swagger.py
```

***

## 🔥 Step 4: Automate Updates with Jenkins

If the docs team wants to **automate this**, add it to **Jenkins**:

1. Open **Jenkins Dashboard** → **New Item** → **Freestyle Project**
2. Name it **Update-Swagger-Docs**
3. Under **Source Code Management**, choose **Git**, and add the **documentation repo URL**
4. Under **Build Triggers**, enable **Poll SCM** (`H/10 * * * *`).
5. Under **Build Steps → Execute Shell**, add:

```sh
#!/bin/bash

cd /var/lib/jenkins/workspace/API-Docs
git pull origin main
python3 update_swagger.py
```

<!-- 
6. **Save & Build Now**  

Now Jenkins will **auto-sync Swagger JSON** whenever the API changes.  

---

## 🎯 Conclusion: Keep Docs Up to Date, Keep Jenkins Happy  

✅ **Docs team can now update Swagger JSON easily**  
✅ **Prevents build failures due to outdated docs**  
✅ **Automates updates using Jenkins**  

With this, your API documentation stays **fresh**, Jenkins stays **green**, and the team stays **happy**. 😃  

Now go forth and **automate your API docs like a pro!** 🚀  

---

## 🔑 Key Takeaways  

| Summary        | Details |
|---------------|---------|
| **Why update Swagger JSON?** | Prevent Jenkins build failures due to outdated API docs. |
| **What does this script do?** | Pulls latest `swagger.json`, updates docs, commits & pushes. |
| **Which libraries are used?** | `GitPython` for Git commands, `jsondiff` for Swagger comparison. |
| **How does Jenkins help?** | Automates API documentation updates. |
| **How to run the script manually?** | `python update_swagger.py` |
| **How to automate it?** | Add to a Jenkins job with `python3 update_swagger.py`. |

```

-->
