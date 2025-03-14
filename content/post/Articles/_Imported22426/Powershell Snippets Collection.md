---
title: Powershell Snippets Collection
description: Colection of Useful Powershell Snippets
slug: Powershell-Snippets
date: 2016-03-22
image: post/Articles/IMAGES/powershell.png
categories:
  - PowerShell
  - Scripting
  - Automation
tags:
  - History
  - PowerShell
  - Scripting
  - Automation
  - Code
  - Samples
draft: false
weight: 49
categories_ref:
  - PowerShell
  - Scripting
  - Automation
lastmod: 2025-03-14T15:45:18.798Z
---
## The Birth of PowerShell

It all began in the early 2000s.

Microsoft was sitting in a cozy little corner, watching the world get all excited about Unix and Linux scripting tools.

They said, “Hey, wouldn’t it be nice if Windows had a powerful command line tool too?” So, in 2006, they released the first version of PowerShell. It wasn’t just another command line, though—it was designed to automate, manage, and interact with pretty much everything in your Windows system.

By 2009, it became open-source, meaning everyone (yes, even you!) could get in on the fun.

### 1. Automate File Management

Tired of moving files manually? Let PowerShell handle that for you. Here's a quick way to copy files based on their extension:

```powershell
Get-ChildItem -Path "C:\Documents" -Filter "*.txt" | Copy-Item -Destination "C:\Backup"
```

This command grabs all `.txt` files from the "Documents" folder and copies them to the "Backup" folder. It's like a personal assistant, but for files.

### 2. Get System Info in a Snap

Ever wondered what’s happening behind the scenes on your system? PowerShell’s got you. Just run:

```powershell
Get-ComputerInfo
```

Boom. A ton of info about your computer, including hardware, OS version, and even the uptime. It's like a quick health check for your machine.

### 3. Download Files from the Web (Yes, Really)

Forget browsing manually for downloads. Let PowerShell download files for you:

```powershell
Invoke-WebRequest -Uri "https://example.com/file.zip" -OutFile "C:\Downloads\file.zip"
```

This command grabs a file from the web and saves it to your local directory. You can automate this, and your downloads will be just a script away!

### 4. Create Users Like a Boss

Need to add a bunch of users to Active Directory? PowerShell will do it without breaking a sweat:

```powershell
New-ADUser -SamAccountName "newuser" -UserPrincipalName "newuser@company.com" -GivenName "New" -Surname "User"
```

In a single line, PowerShell can create an Active Directory user with all their details. Goodbye, manual entry.

### 5. Monitor Processes

When your system’s slowing down, you need to know what’s running. Here’s how you can find the top processes using the most CPU:

```powershell
Get-Process | Sort-Object CPU -Descending | Select-Object -First 5
```

This shows the top 5 processes eating up your CPU. It’s like a bouncer for your system—only letting the important ones in.

### 6. Send Email Alerts

Let’s say your script runs into a problem, and you need to be alerted. No problem:

```powershell
Send-MailMessage -To "you@example.com" -From "script@example.com" -Subject "PowerShell Alert" -Body "Something went wrong" -SmtpServer "smtp.example.com"
```

This sends an email when something goes south. It’s like a mini panic button.

### 7. Schedule Scripts Like a Pro

Want to run a script at 2 AM when no one’s looking? You can use the `Task Scheduler`:

```powershell
$Action = New-ScheduledTaskAction -Execute "powershell.exe" -Argument "C:\Scripts\backup.ps1"
$Trigger = New-ScheduledTaskTrigger -At 2AM -Daily
Register-ScheduledTask -Action $Action -Trigger $Trigger -TaskName "Daily Backup"
```

This schedules your PowerShell script to run at 2 AM every day. No more staying up late for maintenance tasks!

### 8. Backup Your Files

Automate backups with PowerShell! Here's how to back up your "Documents" folder:

```powershell
Copy-Item -Path "C:\Documents" -Destination "E:\Backup" -Recurse
```

This takes all files from your "Documents" folder and copies them to a backup location. It’s like a safety net for your important stuff.

### 9. Monitor Network Traffic

PowerShell can also be your network detective. Here’s a simple way to see active connections:

```powershell
Get-NetTCPConnection
```

You’ll get a list of all active TCP connections on your system, helping you figure out who’s sneaking around your network.

### 10. Remote Administration

Last but not least, you can manage remote computers. For example, to execute a script on a remote server, you can use:

```powershell
Enter-PSSession -ComputerName "Server01" -Credential (Get-Credential)
```

This gives you a session on the remote server, and you can execute commands just as if you were sitting in front of it. PowerShell: now in remote mode!

<!-- 
## Key Ideas

| **Key Idea**                                   | **Description**                                                                 |
|------------------------------------------------|---------------------------------------------------------------------------------|
| Automate File Management                       | Use PowerShell to automate tasks like file copying and moving.                  |
| System Information Retrieval                   | Quickly gather system information with a single command.                        |
| Web Requests and Downloads                     | PowerShell makes downloading files from the web a breeze.                       |
| User Creation and Management                   | Easily create and manage Active Directory users with simple commands.           |
| Process Monitoring                             | Identify high CPU processes and keep track of system performance.               |
| Send Email Alerts                             | Use PowerShell to send automated email alerts based on script outcomes.         |
| Task Scheduling                                | Schedule scripts to run at specific times using the Task Scheduler.            |
| File Backup Automation                         | Automate file backups with just a few lines of PowerShell.                      |
| Network Monitoring                             | Monitor network connections on your system to keep things secure.               |
| Remote Administration                         | Administer remote servers using PowerShell’s remoting capabilities.             |

## References

1. [PowerShell Official Docs](https://docs.microsoft.com/en-us/powershell/)
2. [Microsoft TechNet](https://technet.microsoft.com/en-us/library/hh847931.aspx)
3. [PowerShell GitHub Repository](https://github.com/PowerShell/PowerShell)
4. [Automate with PowerShell on Stack Overflow](https://stackoverflow.com/questions/tagged/powershell)
-->

***

## 11. Manage Windows Services Like a Pro

PowerShell can give you full control over your services. You can start, stop, or restart services with ease. For example, to restart the "Windows Update" service:

```powershell
Restart-Service -Name "wuauserv"
```

This command restarts the Windows Update service, ensuring your system gets the latest updates without you lifting a finger.

## 12. Export Data to CSV (Like a Boss)

Need to export some system information or logs? PowerShell's `Export-Csv` cmdlet is your best friend. For instance, to export all processes to a CSV file:

```powershell
Get-Process | Export-Csv -Path "C:\Processes.csv" -NoTypeInformation
```

This command grabs all running processes and exports them to a CSV file, which you can easily open in Excel. Spreadsheets for the win!

## 13. Find and Kill Zombie Processes

You know those processes that are still running but should've died ages ago? Here's how to kill them:

```powershell
Get-Process -Name "ZombieApp" | Stop-Process
```

If you’ve got any processes named "ZombieApp," this will kill them dead. No more undead apps hanging around!

## 14. Install Software with One Command

Ever wanted to install software but didn’t want to click through the setup wizard? PowerShell can handle that for you. For example, you can install the Google Chrome browser:

```powershell
Start-Process -FilePath "https://dl.google.com/chrome/install/standalonesetup.exe" -ArgumentList "/silent"
```

This command downloads and silently installs Chrome. No user interaction needed—just the way we like it.

## 15. Get Disk Space Usage

Running low on disk space? No problem! Check your disk space with this:

```powershell
Get-PSDrive -PSProvider FileSystem
```

It will list all your drives and their current disk usage. Now you’ll know exactly when to delete all those old files of questionable value (hello, 2014 memes).

## 16. Remotely Manage Computers

One of PowerShell’s coolest features is remoting. To execute commands on a remote computer, use:

```powershell
Invoke-Command -ComputerName "RemotePC" -ScriptBlock { Get-Process }
```

This runs the `Get-Process` command on the remote PC and returns the results. You’re managing computers like a true wizard.

## 17. Monitor Log Files for Specific Entries

You can monitor log files for specific text and even send alerts if something important happens. For instance, if you want to watch the event log for "Error" entries:

```powershell
Get-Content "C:\Logs\SystemLog.txt" -Wait | Select-String "Error"
```

This will continuously check the log and highlight any errors. It’s like a personal security guard for your logs.

## 18. Create Directories Based on Date

Need to organize backups or logs by date? Use PowerShell to create folders based on the current date:

```powershell
$folderName = Get-Date -Format "yyyy-MM-dd"
New-Item -Path "C:\Backups" -Name $folderName -ItemType Directory
```

This creates a new directory every day based on the current date. You’ll never lose track of your backups again.

## 19. Run Multiple Commands Simultaneously

Need to execute multiple commands at once? You can do that with `Start-Job`:

```powershell
Start-Job -ScriptBlock { Get-Process }
Start-Job -ScriptBlock { Get-Service }
```

This runs both `Get-Process` and `Get-Service` commands in parallel, making your tasks more efficient.

## 20. Set Up a Scheduled Task for System Cleanup

You can schedule regular tasks to keep your system tidy. Here’s how you can set up a task to clean up temporary files:

```powershell
$Action = New-ScheduledTaskAction -Execute "powershell.exe" -Argument "C:\Scripts\cleanup.ps1"
$Trigger = New-ScheduledTaskTrigger -Daily -At "3AM"
Register-ScheduledTask -Action $Action -Trigger $Trigger -TaskName "System Cleanup"
```

This task will run a cleanup script at 3 AM every day. Your computer will stay neat and tidy without any effort.

## 21. List All Installed Programs

Need a list of all the software installed on your computer? PowerShell can easily do that:

```powershell
Get-WmiObject -Class Win32_Product | Select-Object Name, Version
```

This command will pull a list of installed software along with their version numbers. It's like your own personal inventory for your computer.

## 22. Rename Files in Bulk

Let’s say you need to rename a bunch of files. No sweat! PowerShell can handle that with a single line. Here's how to rename all `.txt` files by appending the current date to the filename:

```powershell
Get-ChildItem -Path "C:\Files" -Filter "*.txt" | Rename-Item -NewName {$_.Name -replace ".txt", "$(Get-Date -Format 'yyyyMMdd').txt"}
```

This command grabs all `.txt` files in the specified directory and renames them by adding the current date. File management made easy!

## 23. Get a List of Services and Their Status

You know those times when you need to check if services are running? PowerShell has you covered:

```powershell
Get-Service | Select-Object Name, Status
```

This command lists all services and their current status (running, stopped, etc.). You’ll be the service master in no time!

## 24. Automate Your System Cleanup

Your system accumulates temp files, logs, and caches. Let’s automate that cleanup process:

```powershell
Remove-Item "C:\Users\$env:USERNAME\AppData\Local\Temp\*" -Force
```

This cleans out your Temp folder in a snap. You can schedule it to run every so often, and your computer will always be squeaky clean.

## 25. Compress Files into a ZIP Archive

Want to compress a bunch of files into a ZIP archive? Just do this:

```powershell
Compress-Archive -Path "C:\Files\*" -DestinationPath "C:\Backup.zip"
```

This command zips all files in the folder into a single `.zip` archive. Now you can store, share, or back up files without the clutter.

## 26. Send HTTP Requests (Like a Web Developer)

Need to interact with a website or API? PowerShell can send HTTP requests, like so:

```powershell
Invoke-RestMethod -Uri "https://api.example.com/data" -Method Get
```

This sends a GET request to the specified API endpoint. It's like having a web browser inside your terminal!

## 27. Set a Wallpaper with PowerShell

Want to change your desktop wallpaper with a PowerShell script? Here’s how:

```powershell
$wallpaper = "C:\Path\To\Your\Wallpaper.jpg"
Add-Type -TypeDefinition @"
using System;
using System.Runtime.InteropServices;
public class Wallpaper {
    [DllImport("user32.dll", CharSet = CharSet.Auto)]
    public static extern int SystemParametersInfo(int uAction, int uParam, string lpvParam, int fuWinIni);
}
"@
[Wallpaper]::SystemParametersInfo(20, 0, $wallpaper, 0x01 | 0x02)
```

This script changes your wallpaper to the image at the specified path. Want to impress your friends? Change your wallpaper every 5 minutes.

## 28. Monitor System Performance with Real-Time Stats

If you want to keep an eye on your system’s performance in real-time, check out this command:

```powershell
Get-Counter '\Processor(_Total)\% Processor Time'
```

This will show you the CPU usage in real-time. You can even watch memory, disk, and network performance in a similar way. PowerShell as your personal system monitor!

## 29. Fetch the IP Address of Your Machine

Curious about your computer's IP address? Here's how to get it:

```powershell
(Get-NetIPAddress -AddressFamily IPv4 -InterfaceAlias "Ethernet").IPAddress
```

This command will pull your local IP address. It's perfect for remote access or troubleshooting network issues.

## 30. Create and Manage Virtual Machines

PowerShell can even help you manage virtual machines! Here's an example of creating a new VM:

```powershell
New-VM -Name "MyNewVM" -MemoryStartupBytes 2GB -VHDPath "C:\VMs\MyNewVM.vhdx"
```

## 31. Search for Files by Content

Ever needed to search for files that contain a specific word or phrase? PowerShell can help with that:

```powershell
Select-String -Path "C:\Documents\*" -Pattern "confidential"
```

This command will search through all files in the "Documents" folder and find any file that contains the word "confidential." Perfect for tracking down sensitive info or just finding that one file you misplaced.

## 32. Retrieve Disk Space Usage for Each Drive

Wondering how much space you’ve got left on each drive? PowerShell can show you that:

```powershell
Get-PSDrive -PSProvider FileSystem | Select-Object Name, @{Name="Used(GB)";Expression={[math]::round($_.Used/1GB,2)}}, @{Name="Free(GB)";Expression={[math]::round($_.Free/1GB,2)}}, @{Name="Used(%)";Expression={[math]::round(($_.Used/$_.Used+$_.Free)*100,2)}}
```

This one-liner shows each drive’s usage in GB and percentage. No more guessing where all your space went.

## 33. Monitor Disk Health

Let’s face it, no one likes thinking about hard drive failure, but you can monitor the health of your drives:

```powershell
Get-WmiObject -Class Win32_DiskDrive | Select-Object DeviceID, MediaType, Status
```

This checks the health status of all connected drives. If your hard drive is starting to give you the "I'm tired" signal, PowerShell can help you notice early.

## 34. Quickly Rename Multiple Files with a Pattern

Want to quickly rename a bunch of files in a folder using a pattern? PowerShell makes this easy:

```powershell
Get-ChildItem -Path "C:\Files" -Filter "*.txt" | Rename-Item -NewName {$_.Name -replace "oldpattern","newpattern"}
```

This renames all `.txt` files that match the "oldpattern" to a "newpattern." It’s like finding and replacing file names!

## 35. Automatically Send a Daily Email Report

Need to send a daily email report from your system? PowerShell can do that for you. Here’s how you’d send an email with system uptime info:

```powershell
$uptime = (Get-Uptime).ToString()
Send-MailMessage -From "you@domain.com" -To "recipient@domain.com" -Subject "Daily Uptime Report" -Body "System uptime: $uptime" -SmtpServer "smtp.domain.com"
```

This sends an email with the system’s uptime. You could use this to send status reports, logs, or anything else on a regular schedule.

## 36. Use PowerShell to Create a Simple Web Server

Yes, PowerShell can even create a basic web server. Here’s how to start a simple HTTP server on port 8080:

```powershell
$listener = [System.Net.HttpListener]::new()
$listener.Prefixes.Add("http://localhost:8080/")
$listener.Start()
Write-Host "Listening on http://localhost:8080/"
while ($listener.IsListening) {
    $context = $listener.GetContext()
    $response = $context.Response
    $response.ContentType = "text/plain"
    $response.StatusCode = 200
    $response.OutputStream.Write([Text.Encoding]::UTF8.GetBytes("Hello from PowerShell!"))
    $response.OutputStream.Close()
}
```

This creates a basic web server that listens on `localhost:8080` and responds with “Hello from PowerShell!” Super basic, but it shows how flexible PowerShell can be!

## 37. Schedule a Script to Run When You Log In

Want to run a script every time you log in to your computer? Use Task Scheduler to automate that:

```powershell
$Action = New-ScheduledTaskAction -Execute "powershell.exe" -Argument "C:\Scripts\loginscript.ps1"
$Trigger = New-ScheduledTaskTrigger -AtLogon
Register-ScheduledTask -Action $Action -Trigger $Trigger -TaskName "Login Script"
```

This runs your script every time you log in. Perfect for setting up your environment or running a quick task as soon as you start your session.

## 38. Create a Simple GUI for Your Script

If you want to make your script a bit fancier, you can even add a simple GUI! Here’s how to create a basic window:

```powershell
Add-Type -AssemblyName "System.Windows.Forms"
[System.Windows.Forms.MessageBox]::Show('Hello, PowerShell!')
```

This creates a simple message box that pops up with “Hello, PowerShell!” You can create more complex GUIs for your scripts too!

## 39. Generate Random Passwords

Need to generate a strong password for your next secure account? PowerShell can do that:

```powershell
Add-Type -TypeDefinition @"
using System;
public class PasswordGenerator {
    public static string GeneratePassword(int length) {
        const string validChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890!@#$%^&*()";
        Random rand = new Random();
        char[] password = new char[length];
        for (int i = 0; i < length; i++) {
            password[i] = validChars[rand.Next(validChars.Length)];
        }
        return new string(password);
    }
}
"@
[PasswordGenerator]::GeneratePassword(16)
```

This will generate a random 16-character password. You can easily tweak the length and characters for whatever you need.

## 40. Query and Manage Active Directory Users

PowerShell is awesome for interacting with Active Directory. Here’s how to list all users in your domain:

```powershell
Get-ADUser -Filter * | Select-Object Name, SamAccountName
```

This pulls up a list of all Active Directory users, along with their usernames. You can do more than just view users—you can modify, add, and delete them with similar commands.

## 41. View File System Permissions

Want to know who has access to a particular file or folder? PowerShell can show you the permissions:

```powershell
Get-Acl "C:\Path\To\FileOrFolder" | Format-List
```

This command retrieves the permissions for the specified file or folder, so you can see exactly who has access and what kind of access they have. It's like being a file system detective!

## 42. Backup the Windows Registry

You can back up your Windows registry with just one command:

```powershell
reg export "HKCU\Software" "C:\Backup\RegistryBackup.reg"
```

This backs up the "Software" registry key for the current user to a `.reg` file. Perfect for ensuring that your registry settings are safe before making changes.

## 43. Automate Software Updates

Want to make sure your software stays up to date? PowerShell can automate updates for certain programs. For example, here’s how you can update Windows itself:

```powershell
Install-Module PSWindowsUpdate
Get-WindowsUpdate -AcceptAll -Install -AutoReboot
```

This installs the PSWindowsUpdate module and uses it to check for and install any available updates. No more worrying about missing out on critical patches!

## 44. Extract Information from Event Logs

Need to dig into your system’s event logs? PowerShell makes it easy to extract useful info:

```powershell
Get-EventLog -LogName Application -EntryType Error -Newest 10
```

This command gets the 10 most recent error events from the application log. You can customize it to look for different types of events or logs. PowerShell: your personal log investigator.

## 45. Move Files Based on Date

You can organize files by moving them based on their creation or modification date. Here's how you could move files older than a week:

```powershell
Get-ChildItem "C:\Files" | Where-Object {$_.LastWriteTime -lt (Get-Date).AddDays(-7)} | Move-Item -Destination "C:\OldFiles"
```

This command moves any files that haven’t been modified in the last 7 days into the "OldFiles" directory. It’s a great way to clean up and organize!

## 46. Set Environment Variables

Need to set or modify environment variables for your session? PowerShell has you covered:

```powershell
[System.Environment]::SetEnvironmentVariable("MY_VAR", "SomeValue", "User")
```

This command sets the "MY\_VAR" environment variable for the current user. You can also set them system-wide or for just the current process. Perfect for setting custom paths or credentials.

## 47. Get Your Public IP Address

Want to know your public IP address from within PowerShell? Here's how you can check:

```powershell
(Invoke-WebRequest -Uri "http://ipinfo.io/ip").Content
```

This command makes a web request to `ipinfo.io` and retrieves your public IP address. It’s handy for network configurations or just for curiosity’s sake!

## 48. Download Files from the Web with Retry Logic

If you need to download a file from the web but want to make sure the download succeeds, you can add retry logic:

```powershell
$url = "https://example.com/file.zip"
$destination = "C:\Downloads\file.zip"
$retries = 3

for ($i = 0; $i -lt $retries; $i++) {
    try {
        Invoke-WebRequest -Uri $url -OutFile $destination
        Write-Host "Download successful"
        break
    } catch {
        Write-Host "Download failed. Attempt $($i+1) of $retries."
    }
}
```

This script tries to download a file up to 3 times before giving up. If it fails, it retries, which is super helpful when downloading large files or from unreliable servers.

## 49. Convert Files Between Formats

PowerShell can even help with converting files between formats. For example, converting a `.png` image to `.jpg`:

```powershell
Add-Type -AssemblyName System.Drawing
$img = [System.Drawing.Image]::FromFile("C:\Path\To\File.png")
$img.Save("C:\Path\To\File.jpg", [System.Drawing.Imaging.ImageFormat]::Jpeg)
```

This converts a PNG file to JPG format. PowerShell can help you automate file format conversions for all sorts of file types!

## 50. Create and Manage System Snapshots

Want to take a quick snapshot of your system’s state? You can create and manage system restore points:

```powershell
Checkpoint-Computer -Description "Pre-update snapshot" -RestorePointType "MODIFY_SETTINGS"
```

This command creates a restore point before making major changes to your system, like installing updates. If anything goes wrong, you can easily roll back to this snapshot. It’s like giving your system a safety net!

## References

1. [PowerShell Official Documentation](https://docs.microsoft.com/en-us/powershell/)
2. [Managing the Windows Registry with PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/learn/registry)
3. [PowerShell Web Requests](https://docs.microsoft.com/en-us/powershell/scripting/learn/web)
4. [Automating File Conversion with PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/learn/)
5. [System Restore with PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/learn/system-restore)
