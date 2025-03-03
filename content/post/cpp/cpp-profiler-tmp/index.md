---
title: C++ Performance Profiler Library
description: Peformance Profiler Libray- Implemented as a Template Metaprogram
slug: cpp-profiler-tmp
date: 2004-05-06
image: cppprofilercover3.png
categories:
  - CPP
  - Algorithms
  - CPP-Meta Programming
tags:
  - DesignPatterns
  - AspectOrientatedProgramming
  - TemplateProgramming
  - CPP
  - WeavePattern
  - StrategyPattern
weight: 8
published: 2001-01-01
draft: false
lastmod: 2025-03-03T16:30:39.864Z
---
[Template Metaprogramming-Wikipedia](https://en.wikipedia.org/wiki/Template_metaprogramming)

# C++ Performance Profiler-Template Metaprogramming

* library i wrote to use aspect orientated programming weave design pattern to code generate a performance profiler at compile time using template metaprogramming methods.
* Weave is similar to strategy pattern, but strategy implies singular typically

```c++
/*  
 *   Brian Braatz.
 *   Copyright 2003-2022 Brian C Braatz. All rights reserved.
 *
 */

#ifndef __RMX__PROFILE_PROFILE_H
#define __RMX__PROFILE_PROFILE_H

#pragma warning( disable : 4267 )
// STL
#include <time.h>
#include <assert.h>
#include <fstream>
#include <deque>
#include <sstream>
#include <vector>
#include <memory>
#include <set>

// BOOST
#include <boost/bind.hpp>
#include <boost/iostreams/concepts.hpp>      
#include <boost/iostreams/operations.hpp>    
#include <boost/iostreams/filtering_stream.hpp>
#include <boost/iostreams/device/file.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/tokenizer.hpp>
#include <boost/noncopyable.hpp>

#pragma warning( default : 4267 )

// RMX
#ifndef __RMX_STREAMS_H
    #include <Rmx/Streams.H>
#endif

#ifndef __RMX_FLUID_H   
    #include <Rmx/Fluid.h>
#endif

#ifndef __RMX_UTIL_H
    #include <Rmx/Util.h>
#endif

#ifndef _RMX__ERRORS_H
    #include <Rmx/Errors/Errors.H>
#endif
    
#ifndef __RMX__DEBUGOBJECT_H
    #include <Rmx/DebugObject.h>
#endif

#ifndef __BOOST_PROFILER_HPP   
    #include <Profiler/Profiler.hpp>
#endif

#ifndef __RMX_PARAMMAP_H    
    #include <Rmx/ParamMap.h>
#endif    

#ifndef __RMX_CriticalSection_H
    #include <Rmx/CriticalSection.h>
#endif


namespace Rmx /// Rmx Library Namespace
{
    namespace Profile /// Profile Namespace
    {
        namespace detail
        {
            // Intentionally simple Date Time class
            // Its only role is to, upon creation, get the current date time 
            // and allow the user access to the string form of that date and time
            class CurrentDateTime
            {
                    char m_date [9];     // string form of Date i.e. 10/10/05                 
                    char m_time [9];     // string form of Time i.e. 14:41:17  (note: 24 hour format)
                    
                    // internal copy function
                    void CopyFrom(CurrentDateTime const & rSource)
                    {
                        if (this != &rSource) // self assignment check
                        {
                            assert(rSource.IsValid());

                            strcpy(m_date, rSource.m_date);
                            strcpy(m_time, rSource.m_time);

                            assert(IsValid());
                        }
                    }
            public:
                    // DBC Validity checking
                    bool IsValid() const
                    {   
                        size_t iLenDate = strlen(m_date);
                        size_t iLenTime = strlen(m_time);
                        if (
                                iLenDate >0 && iLenDate <=8 &&
                                iLenTime >0 && iLenTime <=8
                        )
                                return true;
                        return false;
                    }
                    
                    // ctor
                    // construct object and retrieve the current date \ time
                    CurrentDateTime()
                    {
                        Update();
                        assert(IsValid());
                    }
                           
                    // copy ctor
                    CurrentDateTime(CurrentDateTime const & rSource)
                    {
                        CopyFrom(rSource);
                        assert(IsValid());
                    }
                  
                    // operator=
                    void operator=(CurrentDateTime const & rSource)
                    {
                        CopyFrom(rSource);
                    }

                    // return a string date
                    std::string Date()  const
                    {
                        assert(IsValid());
                        return m_date;    
                    }
                    // return a string time
                    std::string Time()  const
                    {
                        assert(IsValid());
                        return m_time;
                    }
                    // Update the internal data with the current date and time
                    void Update()
                    {
                        _strdate( m_date );     // retrieve the current date
                        _strtime( m_time );     // retrieve the current time
                    }
            };
        }; // namespace detail

        /// profile errors
        namespace Errors
        {
	       class Error : public Rmx::Errors::Error
	       {
			    public:
                    RMX_ERROR_IMPL(Rmx::Profile::Errors::Error, Rmx::Errors::Error, "General Rmx Profile Library Error");
	       };
	       class InternalError : public Rmx::Profile::Errors::Error
	       {
			    public:
                    RMX_ERROR_IMPL(Rmx::Profile::Errors::InternalError, Rmx::Profile::Errors::Error, "UnExpected internal Profiler error");
		   };
	       class MultipleAppProfilerError : public Rmx::Profile::Errors::Error
	       {
			    public:
                    RMX_ERROR_IMPL(Rmx::Profile::Errors::MultipleAppProfilerError, Rmx::Profile::Errors::Error, "Attempt to install multiple profilers via AppProfilerManager::SetAppProfiler()");
		   };
	       class ProfileLogCreationFailureError : public Rmx::Profile::Errors::Error
	       {
			    public:
                    RMX_ERROR_IMPL(Rmx::Profile::Errors::ProfileLogCreationFailureError, Rmx::Profile::Errors::Error, "Error creating profile log- policy function returned NULL pointer");
		   };
	       class ProfileReportCreationFailureError : public Rmx::Profile::Errors::Error
	       {
			    public:
                    RMX_ERROR_IMPL(Rmx::Profile::Errors::ProfileReportCreationFailureError, Rmx::Profile::Errors::Error, "Error creating profile report- policy function returned NULL pointer");
		   };
 	       class AppProfilerInitializeNotCalled : public Rmx::Profile::Errors::Error
	       {
			    public:
                    RMX_ERROR_IMPL(Rmx::Profile::Errors::AppProfilerInitializeNotCalled, Rmx::Profile::Errors::Error, "AppProfiler.Initialize() must be called prior to creation of any thread profilers");
		   };
  	       class AppProfilerInitializeCalledMoreThanOnce : public Rmx::Profile::Errors::Error
	       {
			    public:
                    RMX_ERROR_IMPL(Rmx::Profile::Errors::AppProfilerInitializeCalledMoreThanOnce, Rmx::Profile::Errors::Error, "AppProfiler.Initialize() must be called ONCE and only once per application");
		   };
	       class AppProfilerParamError : public Rmx::Profile::Errors::Error
	       {
			    public:
                    RMX_ERROR_IMPL(Rmx::Profile::Errors::AppProfilerParamError, Rmx::Profile::Errors::Error, "AppProfiler Parameters incorrect");
		   };
 	       class ProfileDirCreationError : public Rmx::Profile::Errors::Error
	       {
			    public:
                    RMX_ERROR_IMPL(Rmx::Profile::Errors::ProfileDirCreationError, Rmx::Profile::Errors::Error, "Error Creating the configured profile output dir");
		   };
 	       class ProfileDirBlankError : public Rmx::Profile::Errors::Error
	       {
			    public:
                    RMX_ERROR_IMPL(Rmx::Profile::Errors::ProfileDirBlankError, Rmx::Profile::Errors::Error, "Profiler output dir is blank");
		   };
 	       class ThreadProfilerAlreadyDefinedError : public Rmx::Profile::Errors::Error
	       {
			    public:
                    RMX_ERROR_IMPL(Rmx::Profile::Errors::ThreadProfilerAlreadyDefinedError , Rmx::Profile::Errors::Error, "Attempt was made to define a thread profiler, when a profiler for that thread already exists");
		   };
 	       class ProfileEventInvalidLogTypeError : public Rmx::Profile::Errors::Error
	       {
			    public:
                    RMX_ERROR_IMPL(Rmx::Profile::Errors::ProfileEventInvalidLogTypeError , Rmx::Profile::Errors::Error, "The sLogType string passed to ProfileEvent ctor must be exactly 2 chars- no more - no less");
		   };
		   
       }; // namespace Errors
        
        /// Holds the profile event data 
        /// note: basic usage is to create this object and store it upon (RAII) entering a profile event
        ///       elapsed time, is then calculated and store upon (RAII) leaving a profile event
        ///
        /// also note that current date time is calculated automatically upon creation of ProfileEventLogData
        struct ProfileEventLogData : private boost::noncopyable, public Rmx::Debug::DebugObject
        {
            protected:
                std::string                                 m_sName;            /// ProfileEvent name
                double                                      m_elapsed;          /// Elapsed time
                int                                         m_depth;            /// call stack depth of log entry
                long                                        m_lSequenceNumber;  /// Unique sequence number
                std::string                                 m_sEntryType;       /// 2 char log entry type
                detail::CurrentDateTime                     m_EntryDateTime;    /// date and time of entry into the profile event
                boost::shared_ptr<Rmx::Debug::FileLineUniversalId> m_spFluid;   /// FLUID for the file and line where Profile Point was entered

            public:
                // DBC validity checking for this object
                bool IsValid()  const
                {
                    if
                        ( 
                        DebugObjectValid() == true      &&
                        m_sName.length() > 0            &&
                        m_elapsed >=  0                 &&
                        m_depth   >=  0                 && 
                        m_lSequenceNumber >0            &&
                        m_sEntryType.length() == 2      &&
                        m_EntryDateTime.IsValid()       &&
                        m_spFluid.get() != NULL
                        )
                        return true;
                    return false;                
                }
           
                // ctor
                // automatically grabs and saves the current date time upon construction (m_EntryDateTime)
                ProfileEventLogData(    std::string sName, 
                                        int iDepth, 
                                        long const & lSequenceNumber,
                                        std::string const & sEntryType, 
                                        boost::shared_ptr<Rmx::Debug::FileLineUniversalId> & spFluid)
                                        
                :   m_sName(sName),           m_depth(iDepth),    m_lSequenceNumber(lSequenceNumber), 
                    m_sEntryType(sEntryType), m_spFluid(spFluid), m_elapsed(0),    m_EntryDateTime() 
                {  
                    assert(IsValid());
                }
                
                // store the elapsed time in this object
                void SetElapsed(double const & dElapsed)
                {
                    m_elapsed = dElapsed;
                    assert(IsValid());
                }
                
                ////////////////////
                // Access functions

                // Name of the profile event
                std::string const & Name()  const
                {
                    assert(IsValid());
                    return m_sName;
                }
                // Elapsed time for the profile event
                double Elapsed()   const
                {
                    assert(IsValid());
                    return m_elapsed;
                } 
                // Call stack depth for the profile event
                int Depth()  const
                {
                    assert(IsValid());
                    return m_depth;
                }     
                // sequence number for this entry
                long SequenceNumber() const
                {
                    assert(IsValid());
                    return m_lSequenceNumber;
                }
                // 2 char entry type for this entry
                std::string const & EntryType() const
                {
                    assert(IsValid());
                    return m_sEntryType;
                }
                // Date \ time the Profile Event was entered
                detail::CurrentDateTime const & EntryDateTime()  const
                {
                    assert(IsValid());
                    return m_EntryDateTime;    
                }
                // File and Line where the profile event was entered
                Rmx::Debug::FileLineUniversalId const & Fluid()  const
                {
                    assert(IsValid());
                    return *m_spFluid;    
                }
        };

        namespace detail
        {
            // Forward declare for profiler manager class type
            // purposely keeping it old school so reduce lib dependancies
            // (note the whole mechanism below here will be reworked on a subsequent versoin- 
            //  for v1.0 it is more important to reduce extra library dependancies)
            class profile_manager_with_sink;
                
            // Profiler event policy 
            //    allows for registration of profilemanagers
            //    receieves events from the profiler this policy is applied to
            //        when those events are receieved, redirects them to all of the registered profilers
            template
            <
                typename key_t,                                     // Profiler Key Type
                typename duration_t,                                // Profier duration type  
                typename timer_t                                    // Profiler timer type    
            >
            class multi_manager_sink_event_policy
            {
                public:
                    typedef typename key_t      key_type;           // Profiler Key Type
                    typedef typename duration_t duration_type;      // Profier duration type    
                    typedef typename timer_t    timer_type;         // Profiler timer type       
	            protected:
	                std::set<profile_manager_with_sink *>	m_setProfManagers;  // set of profiler managers to receieve notifications from Profile Event objects
	                                                                            // Note- this container tracks RAII objects, hence the weak binding by using a raw ptr type 
	            public:
	                // add a profile manager to receieve notifications
	                void Add(profile_manager_with_sink * pProManager)
	                {
		                // insert into the set using the ptr itself as the key
		                m_setProfManagers.insert(pProManager);
	                }
                	
	                // remove a profile manager to no longer receieve notifications
	                void Remove(profile_manager_with_sink * pProManager)
	                {
		                // remove from the set (again using the ptr value as key)
		                m_setProfManagers.erase(pProManager);
	                }
                protected:        
                    // Policy function call - called by boost profiler when a profile event is starting
                    void on_start(const key_type& key) ;

                    // Policy function call - called by boost profiler when a profile event is stopping
                    void on_stop(const key_type& key, const duration_type& dur, bool underflow, bool overflow) ;
            }; 

            // Typedef weave of applied policies for both the manager and the profiler
            typedef boost::profiling::basic_profile_manager<
                                                                boost::profiling::profile_key, 
                                                                double, 
                                                                boost::high_resolution_timer,
                                                                multi_manager_sink_event_policy
                                                      
                                                            > profile_manager_with_sink_typedef; 

            typedef boost::profiling::basic_profiler<profile_manager_with_sink_typedef> profile_event_typedef;   

            // profiler_manager_t Concrete type which derives from the typedef
            // this is here because you cannot forward declare a typedef
            // and multi_manager_sink_event_policy requires a forward declare
            // (as stated above, this will go away in a subsequent version)
            class profile_manager_with_sink: public profile_manager_with_sink_typedef
            {};

            template 
            <    
                typename key_t,         // Profiler Key Type
                typename duration_t,    // Profier duration type  
                typename timer_t        // Profiler timer type    
            >
            void multi_manager_sink_event_policy
            <
                key_t,                 // Profiler Key Type
                duration_t,            // Profier duration type  
                timer_t                // Profiler timer type    
            >:: on_start(const key_type& key) 
            { 
                using namespace boost;
                // forward the notification to registered profile managers
 	            std::for_each(m_setProfManagers.begin(), m_setProfManagers.end(), bind(&profile_manager_with_sink::on_start,_1,key) );
            }

            template 
            <   
                typename key_t,         // Profiler Key Type
                typename duration_t,    // Profier duration type  
                typename timer_t        // Profiler timer type    
            >
            void multi_manager_sink_event_policy
            <
                key_t,                  // Profiler Key Type
                duration_t,             // Profier duration type  
                timer_t                 // Profiler timer type    
            >:: on_stop(const key_type& key, const duration_type& dur, bool underflow, bool overflow) 
            { 
                // forward the call to registered profile managers
	            std::for_each(m_setProfManagers.begin(), m_setProfManagers.end(), boost::bind(&profile_manager_with_sink::on_stop,_1,key,dur,underflow,overflow) );
            }

        }; // namespace detail
        
        namespace Policies
        { 
            // Default log policy
            class Default_ProfileLog_ReportPolicy
            {
                public:
                    typedef std::ofstream    Log_StreamValueType;
                    
                protected:        
                    // Called to create a thread log
                    Log_StreamValueType * LogCreate(std::string const & sDir, std::string const & sReportName, std::string const & sEntity, long lThreadId )
                    {
                        std::stringstream ssFileName;
                        // build the filename
                        ssFileName << sDir << '\\' << sEntity << '_' << sReportName << '_' << lThreadId << ".txt";

                        // create a new logfile object
                        std::ofstream * pLogFile = new std::ofstream;
                         
                        // open the file (append)
                        pLogFile->open (ssFileName.str().c_str(), ios::app);   
                        
                        if (pLogFile->is_open() == false) // if the file failed to open return null
                        {
                            boost::checked_delete(pLogFile); 
                            pLogFile = NULL;   
                        }
                        else    // file is open
                        {
                            // turn off scientific notation of small numbers
                            *pLogFile  << std::setiosflags( std::ios_base::fixed );
                        }      
                        return pLogFile;
                    }
                    // Called to close out the log
                    void LogClose(Log_StreamValueType & LogFile)  
                    {
                        LogFile.close(); 
                    }
                    // Called to stream the header of the beginning of the log
                    void LogStreamHeader(Log_StreamValueType & out) const
                    {
                        out << "DATE\tTIME\tSEQ\tTHREAD\tDEPTH\tTYPE\tNAME\tELAPSED\tFILE\tLINE\tFULLFILE" << std::endl;          
                        out << "" << std::endl;
                    }  
               
                    // called to stream a Detail log line to the log report 
                    void LogStreamDetailLine(Log_StreamValueType & LogFile, std::string const & sThreadLongName, Rmx::Profile::ProfileEventLogData const & evtLogData) 
                    {
                        LogFile     << evtLogData.EntryDateTime().Date() << '\t' 
                                    << evtLogData.EntryDateTime().Time() << '\t' 
                                    << evtLogData.SequenceNumber() << '\t'
                                    << sThreadLongName  << '\t' 
                                    << evtLogData.Depth() << '\t' 
                                    << evtLogData.EntryType() << '\t' 
                                    << evtLogData.Name() << '\t' 
                                    << evtLogData.Elapsed()<< '\t'
                                    <<  evtLogData.Fluid().FileName(Rmx::Debug::FileLineUniversalId::SIMPLE) << '\t' 
                                    <<  evtLogData.Fluid().LineStr() << '\t' 
                                    <<  evtLogData.Fluid().FileName(Rmx::Debug::FileLineUniversalId::FULL) << '\t'
                                    << std::endl;
                    }     
                protected:
                    // protected dtor to stop non-inherited useage
                    ~Default_ProfileLog_ReportPolicy()
                    {   }
            };
            // default reporting policy for app profiling
            class Default_ProfileReport_ReportPolicy 
            {
                protected:
                    typedef std::ofstream    Report_StreamValueType;   

                    Report_StreamValueType * ProfileReportCreate(std::string const & sDir, std::string const & sReportName, std::string const & sEntity, long lThreadId )
                    {
                        std::stringstream ssFileName;
                        // build the filename
                        ssFileName << sDir << '\\' << sEntity << '_' << sReportName << '_' << lThreadId << ".txt";
 
                        Report_StreamValueType* pofProReport  = new std::ofstream(ssFileName.str().c_str(),ios::app);
                        // if file fails to open, make sure we clean up and return NULL
                        if (pofProReport->is_open() == false)
                        {
                            boost::checked_delete(pofProReport);
                            pofProReport = NULL;   
                        }
                        return pofProReport;    
                    }
                    // stream the header, using the thread long name and the entity requesting the report
                    void ProfileReportStreamHeader(std::string const & sThreadLongName, std::string const & sEntity, std::ostream & out)
                    {
                        detail::CurrentDateTime datetimenow;    // get current time
                        out   << sThreadLongName << "\t" << sEntity << "\t" << datetimenow.Date() << "\t" << datetimenow.Time()<<  endl;  
                    }
                    void ProfileReportClose(Report_StreamValueType & outProfReport)  
                    {
                        // close the file
                        outProfReport.close();
                    }
                protected:
                    // protected dtor to stop non-inherited useage
                    ~Default_ProfileReport_ReportPolicy()
                    {   }
            };
            // default reporting policy for Measured Scopes
            class Default_MSProfileReport_ReportPolicy 
            {
                protected:
                    typedef std::ofstream    Report_StreamValueType;

                    Report_StreamValueType * ProfileReportCreate(std::string const & sDir, std::string const & sReportName, std::string const & sEntity, long lThreadId )
                    {
                        std::stringstream ssFileName;
                        // build the filename
                        ssFileName << sDir << '\\' << sEntity << '_' << sReportName << '_' << lThreadId << ".txt";

                        Report_StreamValueType* pofProReport  = new std::ofstream (ssFileName.str().c_str(),ios::app);        
                        
                        // if file fails to open, make sure we clean up and return NULL
                        if (pofProReport->is_open() == false)
                        {
                            boost::checked_delete(pofProReport);
                            pofProReport = NULL;   
                        }
                        else    // file is open
                        {
                            // turn off scientific notation of small numbers
                            *pofProReport  << std::setiosflags( std::ios_base::fixed );
                        }      
                        return pofProReport;
                    }
                    // stream the header, using the thread long name and the entity requesting the report
                    void ProfileReportStreamHeader(std::string const & sThreadLongName, std::string const & sEntity, std::ostream & out)
                    {
                        detail::CurrentDateTime datetimenow;    // get current time
                        out   << sThreadLongName << "\t" << sEntity << "\t" << datetimenow.Date() << "\t" << datetimenow.Time()<<  endl;  
                    }
                   
                    void ProfileReportClose(Report_StreamValueType &ofProReport)
                    {
                        // Close the file
                        ofProReport.close();
                    }
                protected:
                    // protected dtor to stop non-inherited useage
                    ~Default_MSProfileReport_ReportPolicy()
                    {   }
            };
        };  // namespace Policies
    
        // forward declare appprofiler
        template <typename ThreadProfiler_t>
        class AppProfiler;
        
        template <  typename    ProfileLog_ReportPolicy     = Policies::Default_ProfileLog_ReportPolicy,
                    typename    ProfileReport_ReportPolicy = Policies::Default_ProfileReport_ReportPolicy >  
        // Thread Profiler
        class ThreadProfiler :  public      detail::profile_manager_with_sink ,  
                                private     boost::noncopyable,  
                                public      Rmx::Debug::DebugObject,
                                public      ProfileLog_ReportPolicy,
                                public      ProfileReport_ReportPolicy
        {
            public:
                // typedef for container of ProfileEvent data logs
                typedef std::vector< boost:: shared_ptr <ProfileEventLogData> >             ProfileEventLogDataContainerType;
                // typdef for the output type
                typedef typename ProfileLog_ReportPolicy::Log_StreamValueType				Log_StreamValueType;
                
            private:
                ProfileEventLogDataContainerType                        m_ProfileEventLogData;     // container for profilerevent logs
                int                                                     m_iDepth ;                 // current call stack depth for this thread
                boost::shared_ptr<Log_StreamValueType>                  m_pLogFile;                // log file
                DWORD                                                   m_dThreadId;               // ThreadId for this thread
                std::string                                             m_sThreadName;             // app code assigned name for this thread
                AppProfiler<ThreadProfiler>    *                        m_pAppProf;                // app profiler          
				bool													m_bProfilerHasBeenTornDown;// true if the profiler has been torn down
                Rmx::Debug::FileLineUniversalId                         m_fluid;                   // fluid for where this thread was named
                std::string                                             m_sThreadLongName;         // Long name of Thead i.e. Main.7823
            public:
                // DBC validity checking
                bool IsValid()
                {
                    if ( 
                            DebugObjectValid()                  == true         &&
                            &m_iDepth                           >0              &&
                            m_pAppProf                          != NULL         &&
                            m_pAppProf->DebugObjectValid()      == true         &&
                            m_sThreadName.length()              != 0            &&
                            m_dThreadId                         != 0            &&
                            Rmx::Platform::GetCurrentThreadId() == m_dThreadId  &&
                            m_fluid.IsEmpty()                   != true         &&
                            m_sThreadLongName.length()          >0          
                        )
                        return true;
                    return false;
                }
                // ctor
                ThreadProfiler(AppProfiler<ThreadProfiler> * pAppProfiler, DWORD dThreadId,std::string sName, Rmx::Debug::FileLineUniversalId const & fluid)
                    throw (Rmx::Profile::Errors::ProfileLogCreationFailureError)    
                    :   m_dThreadId(dThreadId), 
                        m_sThreadName(sName), 
                        m_pAppProf(pAppProfiler), 
                        m_bProfilerHasBeenTornDown(false), 
                        m_fluid(fluid)
                
                {  
                    {   // create and store the thread long name
                        std::stringstream ssThreadLongName;
                        ssThreadLongName << m_sThreadName << '.' << m_dThreadId;
                        m_sThreadLongName = ssThreadLongName.str(); 
                    }
                    assert(IsValid() == true );
                    assert(sName.length() >0);
                    assert(pAppProfiler);
                    assert(m_pAppProf->DebugObjectValid());
                    
                    // reserve some data for the vector (performance)
                    m_ProfileEventLogData.reserve(50);
                    
                    // Create the log via policy- store returned log in shared_ptr
                    m_pLogFile = boost::shared_ptr<Log_StreamValueType>
                                        (ProfileLog_ReportPolicy::LogCreate(pAppProfiler->Settings.OutputDir(),pAppProfiler->Settings.LogFileName(), sName,m_dThreadId));      
                    // if we did not receieve back an object from the policy- throw an error
                    if (m_pLogFile.get() == NULL)
                        throw Rmx::Profile::Errors::ProfileLogCreationFailureError(FLUID);
                        
                    // Stream the log header
                    ProfileLog_ReportPolicy::LogStreamHeader(*(m_pLogFile.get()));

                    m_iDepth = 0;
                }
                // returns the name of this thread
                // (either assigned specifically, or the string form of thread id)
                std::string const & ThreadName()
                {                                                                           
                    assert(IsValid() == true );
                    assert(m_bProfilerHasBeenTornDown == false);
                    return m_sThreadName;   
                }
                // returns the thread long name
                // either Name.Threadid or ThreadId.ThreadId (unnamed thread)
                std::string const & ThreadLongName()
                {
                    assert(IsValid() == true );
                    assert(m_bProfilerHasBeenTornDown == false);
                    return m_sThreadLongName;
                }
                
                DWORD ThreadId()
                {
                    assert(IsValid() == true );
                    assert(m_bProfilerHasBeenTornDown == false);
                    return m_dThreadId;   
                }
                // calls Policy::StreamDetailLogLine() passing in the log file
                void ReportDetailLogLine(Rmx::Profile::ProfileEventLogData & evtLogData)  throw (Rmx::Profile::Errors::InternalError)
                {
                    assert(IsValid() == true );
                    assert(m_bProfilerHasBeenTornDown == false);

                    // double check to make sure we have a valid object 
                    if (m_pLogFile.get() != NULL)
                    {
                        ProfileLog_ReportPolicy::LogStreamDetailLine(*m_pLogFile.get(),m_sThreadLongName, evtLogData);  
                    }
                    else
                    {
                        throw Rmx::Profile::Errors::InternalError(FLUID); // this should never happen-if it does something is horked
                    }
                }
                // tears down the profiling of a thread
                // if the calling thread is NOT the profiler for this thread
                // the thread will be suspended
                void TearDownProfiling()
                {
                    assert(DebugObjectValid() == true );
                    
                    // ensure we never suspend a ThreadProfiler on the same thread we are deleting on
                    if (Rmx::Platform::GetCurrentThreadId() != m_dThreadId)
                    {   
                        // Warn the developer about the need for the dtor to suspend the thread
                        // (note: if you got here it means you have a thread that needs to EXIT before the AppProfiler dtor is called)
                        Rmx::Warnings::RuntimeWarning(std::string("ThreadProfiler dtor for Thread ") 
                                                                + m_sThreadName + 
                                                                ". Thread still running! Suspending thread for clean exit of app",
                                                                FLUID);                                                        
                        Rmx::Warnings::RuntimeMessage(std::string("Thread ") 
                                                                + m_sThreadName + 
                                                                " was NAMED on associated line",
                                                                m_fluid);                                                        
                        // suspend the thread        
                        Rmx::Platform::SuspendThread(m_dThreadId);
                    }
                    // if there IS a thread log, close it
                    if (m_pLogFile.get() != NULL)    
                        ProfileLog_ReportPolicy::LogClose(*m_pLogFile);
                    
                    // set flag denoting whether thread profiling has been torn down
                    m_bProfilerHasBeenTornDown = true;
                }
                 // dtor
                ~ThreadProfiler()
				{
					// if we have not yet been torn down, do so
					if(m_bProfilerHasBeenTornDown == false)
					{
						TearDownProfiling();
					}
				
				}
               // returns a reference to the profile event log
                ProfileEventLogDataContainerType & LogStack()
                {
                    assert(IsValid() == true );
                    assert(m_bProfilerHasBeenTornDown == false);

                    return m_ProfileEventLogData;
                }
                // returns a reference to the call stack depth tracker
                int & GetDepth()
                {
                    assert(IsValid() == true );
                    assert(m_bProfilerHasBeenTornDown == false);

                    return m_iDepth;
                }
                // returns a reference to the log file
                std::ostream & LogFile()
                {
                    assert(IsValid() == true );
                    assert(m_pLogFile.get() != NULL);      // todo error
                    assert(m_bProfilerHasBeenTornDown == false);
                    return *m_pLogFile;
                }
              
                // generate the profile report
                // using passed ostream interface (managed by the calling code)
                void GenerateProfileReport(std::string sNameOfRequestor, std::ostream & out)
                {
                    assert(m_pAppProf->DebugObjectValid());
                    assert(IsValid() == true );
                    assert(m_bProfilerHasBeenTornDown == false);
               
                    // Generate the header
                    ProfileReport_ReportPolicy::ProfileReportStreamHeader(ThreadLongName(), sNameOfRequestor, out);
                    
                    // generate the report
                    detail::profile_manager_with_sink::generate_report(out); 
                
                }
                // generate the profile report
                // using the supplied policy class to manage the output stream
                void GenerateProfileReport(std::string sNameOfRequestor)    throw (Rmx::Profile::Errors::ProfileReportCreationFailureError)
                {
                    assert(m_pAppProf->DebugObjectValid());
                    assert(IsValid() == true );
                    assert(m_bProfilerHasBeenTornDown == false);

                    ProfileReport_ReportPolicy::Report_StreamValueType * pOut = NULL;
                
                    // get the report obj from the policy
                    pOut=  ProfileReport_ReportPolicy::ProfileReportCreate( m_pAppProf->Settings.OutputDir(),
                                                                            m_pAppProf->Settings.ReportFileName(),
                                                                            m_sThreadName,  
                                                                            m_dThreadId 
                                                                           );
                
                    // if one was received
                    if (pOut != NULL)
                    {
                        // Call the overloaded version of this function
                        // this function will generate the actual report with header 
                        GenerateProfileReport(sNameOfRequestor, *pOut);

                        // Close the file
                        ProfileReport_ReportPolicy::ProfileReportClose(*pOut);
                        
                        pOut = NULL;            
                    }
                    else
                    {
                        // otherwise this is an error
                        throw Rmx::Profile::Errors::ProfileReportCreationFailureError(FLUID);
                    }
                }
                
        };


        // Provides a simple wrapper over static functions
        // goal is to ease the interface for a function to get to the app profiler
        // this is to enforce a simple singleton model 
        template <typename ThreadProfiler_t>
        class AppProfilerManager :  public  Rmx::Debug::DebugObject,
                                    private boost::noncopyable  
        {
                static  AppProfiler<ThreadProfiler_t> *     m_AppProf  ;        // static app profiler
            public:
                // ctor
                AppProfilerManager()
                { }
               
                // retrieve app profiler
                static  AppProfiler<ThreadProfiler_t>* GetAppProfiler()
                {
                    return m_AppProf;
                }
                
                // only retrieve if profiler exists and is enabled
                static  AppProfiler<ThreadProfiler_t>* GetAppProfilerIfEnabled();

                // Install the app profiler or uninstall
                // will throw if one is already installed
                static  SetAppProfiler(AppProfiler<ThreadProfiler_t> * pProf) throw (Errors::MultipleAppProfilerError)
                {
                    static CriticalSection csAppMgr;
                    {  
                        CriticalSection::Enter cseAddAppProfiler(csAppMgr);  // critical section to protect adding profilers
                        
                        // No more than one profiler maybe installed at a time
                        if (!(m_AppProf == NULL || pProf == NULL))
                            throw Errors::MultipleAppProfilerError(FLUID);

                        m_AppProf = pProf;
                    }
                }
           private:
            // private dtor
            ~AppProfilerManager()
            {   }
        };
        
        template <typename ThreadProfiler_t> AppProfiler<ThreadProfiler_t> * AppProfilerManager<ThreadProfiler_t>::m_AppProf = NULL;

        // AppProfiler
        // holds all of the thread specific profilers							
        // passes out\ creates the profilers as needed for threads
        template <typename ThreadProfiler_t>
        class AppProfiler  :    public Rmx::Debug::DebugObject,
                                private boost::noncopyable
        {
                typedef std::map<DWORD,boost::shared_ptr<ThreadProfiler_t> >     MapProfilers_t;             
                MapProfilers_t                                                   m_mappThreadProfilers;      // map of thread profilers, keyed by thread id
                bool                                                             m_bEnabled;                 // flag for if profiling is enabled
                ParamMap                                                         m_pmSettings;              // ParamMap of settings
                bool                                                             m_bInitCalled;             // Tracks to make sure init was called
                long                                                volatile     m_lSequenceNumber;          // sequence number for all profilers 
                CriticalSection                                                  m_csMapProfilers;           // critical section for guarding against bad use of MapProfilers

                // Attempts to locate the thread profiler for the passed id
                // if found returns, if not found, returns a null ptr
                boost::shared_ptr<ThreadProfiler_t> LocateThreadProfiler(DWORD const dwThreadId) 
                {
                    assert(DebugObjectValid() == true );

                    // ptr for receiving the found or created profiler
                    boost::shared_ptr<ThreadProfiler_t> retProf;
                    
                    // Iter for traversal of profilers in map
                    MapProfilers_t::iterator itFound;
                    
                    // cs here to guard against map being in the middle of a realloc while searching
                    {   CriticalSection::Enter csGaurdMapProfilers(m_csMapProfilers);

                        // Find the registered profiler for passed thread id
                        itFound = m_mappThreadProfilers.find(dwThreadId);            
                    }
                    
                    // if we found a profiler for this thread
                    if (itFound != m_mappThreadProfilers.end())
                        retProf =(itFound->second);

                    return retProf;
                }
                
                // Creates a new thread profiler under passed dwThreadId
                // may throw 
                boost::shared_ptr<ThreadProfiler_t> CreateAndAddThreadProfiler( DWORD const dwThreadId,                         // Thread id for this thread
                                                                                std::string sName,                              // name for thread- if blank, Thread ID will be used
                                                                                Rmx::Debug::FileLineUniversalId const & fluid)  // FLUID for where the thread was named
                throw (Profile::Errors::AppProfilerInitializeNotCalled)
                {
                    assert(DebugObjectValid() == true );
                    assert(LocateThreadProfiler(dwThreadId).get() == NULL);     // Debug check to ensure the thread profiler does not already exist   todo- throw here?
                    std::string sNameForNewProfiler = sName;                    // name used for new profiler
                    boost::shared_ptr<ThreadProfiler_t> retProf;                // ptr for receiving the found or created profiler
                    
                    // check to make sure Initialize was called before this function
                    // this is important as Initialize() sets up the subdir
                    // NOTE: IF BELOW THROWS FOR YOU- YOU MIGHT HAVE ACCIDENTIALLY CALLED NAME_THREAD before Initialize()
                    if (m_bInitCalled == false)
                        throw Profile::Errors::AppProfilerInitializeNotCalled(FLUID);
                    
                    // if there was a blank name passed, assign the name
                    // as the thread id
                    if (sNameForNewProfiler.length() == 0)
                    {
                        // build a string form of the name using a human readable version of the thread id for the name
                        std::stringstream ssName; ssName << dwThreadId;
                        
                        // save the thread id in the string
                        sNameForNewProfiler = ssName.str();
                    }
                    // create a new thread profiler
                    retProf = boost::shared_ptr<ThreadProfiler_t>(new ThreadProfiler_t(this, dwThreadId, sNameForNewProfiler, fluid));
                    
                     // cs here to guard the map while adding
                    {   CriticalSection::Enter csGaurdMapProfilers(m_csMapProfilers);
                        
                        // add thread profiler to the map
                        m_mappThreadProfilers.insert(std::make_pair(dwThreadId,retProf));
                    }
                    return retProf; 
                }
                
            public:
                  // Helper class for providing clean and unmodifyable access to settings
                class SettingsClass :  private boost::noncopyable
                {
                        // ref to parammap
                        ParamMap    & m_pmSettings;

                        // ctor
                        SettingsClass(ParamMap    & pmSettings)
                        : m_pmSettings(pmSettings)  {}

                        friend class AppProfiler; 
                    public:
                        // returns the ThreadProfileLogFileName parameter
                        std::string const & LogFileName()
                        {
                            return m_pmSettings["ThreadProfileLogFileName"];
                        }
                        // returns the ThreadProfileReportFileName
                        std::string const & ReportFileName()
                        {
                            return m_pmSettings["ThreadProfileReportFileName"];
                        }   

                        // returns the ThreadScopedProfileReportfileName
                        std::string const & ScopedReportFileName()
                        {
                            return m_pmSettings["ThreadScopedProfileReportfileName"];
                        }   
                        // returns the ThreadProfileReportFileName
                        std::string const & OutputDir()
                        {
                            return m_pmSettings["OutputDir"];
                        }  
                    private:
                        ~SettingsClass() 
                        {       }
                } Settings;
                
                AppProfiler()
                : Settings(m_pmSettings), m_bInitCalled(false), m_bEnabled(false)
                {
                    assert(DebugObjectValid() == true );
                    // init sequence number
                    m_lSequenceNumber   = 0;

                    // Install "this" as the app profiler
                    AppProfilerManager<ThreadProfiler_t>::SetAppProfiler(this);

                    ///////////////////////////////////////////////////
                    // Set up the param map
                    // Params and their defaults
                    // ThreadProfileLogFileName             = "ProfileLog"
                    // ThreadProfileReportFileName          = "ProfileReport"
                    // ThreadScopedProfileReportfileName    = "ScopedProfileReport"
                    // OutputDir                            = ".\\ProfileOutput"
                    m_pmSettings.installParam("ThreadProfileLogFileName",               "ProfileLog",true);
                    m_pmSettings.installParam("ThreadProfileReportFileName",            "ProfileReport",true);
                    m_pmSettings.installParam("ThreadScopedProfileReportfileName",      "ScopedProfileReport",true);
                    m_pmSettings.installParam("OutputDir",                              ".\\ProfileOutput",true);
                    m_pmSettings.applyParamDefaults();
                }
                // Intialize profiling for the application
                //  must be called before any logging or profiling takes place
                //  deletes any old output dir, and creates a new one
                void Initialize() throw (Profile::Errors::ProfileDirCreationError)
                {
                    // check to make sure we have not been called before
                    if (m_bInitCalled == true)
                        throw Profile::Errors::AppProfilerInitializeCalledMoreThanOnce(FLUID);                            
                   
                     // Validate the parameters in the map
                    bool bValidParams = m_pmSettings.verifyParams();
                 
                    // error if the params are not valid, 
                    if (bValidParams == false)
                       throw Profile::Errors::AppProfilerParamError(FLUID);
                   
                   // retrieve the output dir
                   std::string sProfileOutDir = Settings.OutputDir();
                    
                    // check to make sure we have a value
                    if (sProfileOutDir.length() == 0)
                        throw Profile::Errors::ProfileDirBlankError(FLUID);
                        
                    // Delete the old dir if it is still around
                    Platform::DeleteDirectory(sProfileOutDir); 
                   
                   // check to make sure the dir was deleted
                   // IF YOU GET THIS ERROR- 
                   // Check to make sure you do not have a file open in this dir or a cmd prompt in this dir
                    if (Platform::FileExists(sProfileOutDir) == true)
                        throw Rmx::Profile::Errors::ProfileDirCreationError(FLUID);    
                    
                    // create the profile output dir
                    bool bOk =  Platform::CreateDirectory(sProfileOutDir);      
                    
                    // If create directory fails, we throw an error
                    if (bOk == false || Platform::FileExists(sProfileOutDir) == false)
                        throw Rmx::Profile::Errors::ProfileDirCreationError(FLUID);    

                    // Set flag denoting init success
                    m_bInitCalled = true;  
                }
                // Intialize the profiler
                // Loads parameters, and then calls Initialize()
                void Initialize(std::istream & instream) throw (Profile::Errors::AppProfilerParamError)
                {
                    // load parameters from stream
                    bool bParamsok = m_pmSettings.loadParams(instream,false);

                    // check to make sure loadParams is ok
                    if (bParamsok == false)
                        throw Profiler::Errors::AppProfilerParamError(FLUID);

                    // Validate the parameters in the map
                    bool bValidParams = m_pmSettings.verifyParams();
                 
                    // error if the params are not valid, 
                    if (bValidParams == false)
                       throw Profile::Errors::AppProfilerParamError(FLUID);

                    // call raw init func
                    Initialize();    
                }
                 // Intialize the profiler
                // Loads parameters, and then calls Initialize()
                void Initialize(std::string const & instr)  throw (Profile::Errors::AppProfilerParamError)
                {
                    // load parameters from stream
                    bool bParamsok = m_pmSettings.loadParams(instr,false);
                    
                    // check to make sure loadParams is ok
                    if (bParamsok == false)
                        throw Profile::Errors::AppProfilerParamError(FLUID);

                    // Validate the parameters in the map
                    bool bValidParams = m_pmSettings.verifyParams();
                 
                    // error if the params are not valid, 
                    if (bValidParams == false)
                       throw Profile::Errors::AppProfilerParamError(FLUID);

                    // call raw init func
                    Initialize();    
                }

                // return the next sequence number
                // this is used for the profile logs 
                // this value will always be unique across all threads profiling
                long GetNextSequenceNumber()
                {
                    InterlockedIncrement(&m_lSequenceNumber);
                    return m_lSequenceNumber;                                    
                }

               ~AppProfiler()
                {
					using namespace boost;
					 // cs here to guard the map while we are busy suspending threads
                    {   CriticalSection::Enter csGaurdMapProfilers(m_csMapProfilers);

					    assert(DebugObjectValid() == true );
					    MapProfilers_t::iterator it;
					    // shut down all of the threads on the profilers if they are running
					    // note: ORDER is very important here -we must shut DOWN the thread profilers
					    // prior to calling SetAppProfiler to null- if this is not done
					    // there is a potential for crashing on a thread trying to use the app profiler
					    for (it = m_mappThreadProfilers.begin(); it != m_mappThreadProfilers.end(); it++)
					    {
						    RMX_ASSERTDEBUGOBJPTR(it->second.get());
						    // tell the profiler to tear down
						    it->second.get()->TearDownProfiling();
					    }

					    // remove this object as a app profiler
					    AppProfilerManager<ThreadProfiler_t>::SetAppProfiler(NULL);
                    }
                }
                // Retrieve thread profiler 
                // will create one if one does not already exist
                // the thread will be named with the string form of the thread id
                // if you do not wish this funcitonality, make sure you call DefineThread() 
                // prior to calling GetThreadProfiler()
                ThreadProfiler_t & GetThreadProfiler()      
                {
                    assert(DebugObjectValid() == true );

                   // ptr for receiving the found or created profiler
                    boost::shared_ptr<ThreadProfiler_t> retProf;

                    // Get the current thread ID
                    DWORD dThreadId = Rmx::Platform::GetCurrentThreadId();        
                    
                    // call helper to lookup thread profiler
                    retProf = LocateThreadProfiler(dThreadId);
                    
                    // if no profiler found- (NO NAME ASSGINED ON THIS THREAD YET)
                    if (retProf.get() == NULL)
                    {
                        // create profiler and add to internal map
                        // usintg the thread id for the name and the BELOW line for the creation FLUID
                        retProf = CreateAndAddThreadProfiler(dThreadId,"",FLUID);
                    }
                    return *retProf;
                }
                
                // Allows a calling thread to name itself
                // will throw an error if GetThreadProfiler() has been called 
                // prior to definethread
                void DefineThread(std::string const & sName, Rmx::Debug::FileLineUniversalId const & fluid)
                {
                    assert(DebugObjectValid() == true );

                    boost::shared_ptr<ThreadProfiler_t> thrdProf;

                    // Get the current thread ID
                    DWORD dThreadId = GetCurrentThreadId();        

                    // call helper to lookup thread profiler
                    thrdProf = LocateThreadProfiler(dThreadId);

                    // if we did not find a profiler with this name (what we expect)
                    if (thrdProf.get() == NULL)
			        {
                        // create profiler and add to internal map
                        CreateAndAddThreadProfiler(dThreadId, sName, fluid);
			        }
			        else
			        {
                        // should never get here
				        Rmx::Warnings::RuntimeWarning("Attempted to define thread " + sName + " Thread already defined as " + thrdProf->ThreadName() + "(See associated file\\line for where duplicate naming occurred)",fluid);
				        throw Profile::Errors::ThreadProfilerAlreadyDefinedError(FLUID);
			        }
                }
                // sets profiling enabled and disabled based on passed setting
                void SetEnabled(bool const benable = true)
                {
                    assert(DebugObjectValid() == true );
      
                    m_bEnabled = benable;
                }
                // returns if profiling is enabled
                bool Enabled() const
                {
                    assert(DebugObjectValid() == true );
                    
                    return m_bEnabled;
                }
        };
        // returns a profiler ptr if there is a profiler and it is enabled
        template <typename ThreadProfiler_t>
        AppProfiler<ThreadProfiler_t>* AppProfilerManager<ThreadProfiler_t>::GetAppProfilerIfEnabled()   
        {
            if( m_AppProf && m_AppProf->Enabled() == true)
            {
                assert(m_AppProf->DebugObjectValid() == true );

                return m_AppProf ;
            }

            return NULL;            
        }
       
        // RAII class for profile events
        // will report to the profiler for this thread
        // and any Measured Scope profile events that maybe alive for this thread
        template <typename ThreadProfiler_t>
        class ProfileEvent  :   public  detail::profile_event_typedef ,  
                                public  Rmx::Debug::DebugObject,
                                private boost::noncopyable
        {        
            protected:
                ThreadProfiler_t                   &                m_ThreadProfiler;       // Reference to thread profiler
                boost::shared_ptr<Rmx::Debug::FileLineUniversalId>  m_spFluid;              // ptr to FLUID for this event         
                std::string								            m_sName;                // string name for this event
                boost::shared_ptr<ProfileEventLogData>              m_pData;                // profile event data for this event
                std::string                                         m_sLogType;             // 2 char Type for profile Event
            public:
                
                // DBC validity checking
                bool IsValid()
                {
                    if ( 
                            DebugObjectValid() == true  &&
                            &m_ThreadProfiler   != NULL &&
                            m_spFluid.get()     != NULL &&
                            m_sName.length()    != 0    &&
                            m_pData.get()       != NULL &&
                            m_sLogType.length() == 2
                        )
                        return true;
                    return false;
                }
                
                // ctor
                // takes a thread profiler, a name, an a ptr to the fluid for this event
                ProfileEvent(ThreadProfiler_t & threadProfiler, std::string sName, boost::shared_ptr<Rmx::Debug::FileLineUniversalId> spFluid, std::string sLogType = "PE")
                : m_ThreadProfiler(threadProfiler) , m_sName(sName), m_spFluid(spFluid), m_sLogType(sLogType), detail::profile_event_typedef(sName, &threadProfiler)   
                {
                    #ifdef RMX_DEBUG    
                        // LOG type is exactly 2 chars- no more - no less
                        if (m_sLogType.length() != 2)
                            // condition not met, throw error
                            throw Rmx::Profile::Errors::ProfileEventInvalidLogTypeError(FLUID);   
                    #endif                    
                    // Increment the depth of the call stack variable on this thread
                    m_ThreadProfiler.GetDepth()++;
                    
                    // double check we have an app profiler
                    if (!Rmx::Profile::AppProfilerManager<ThreadProfiler_t >::GetAppProfiler())
                        throw Rmx::Profile::Errors::InternalError(FLUID);
                    
                    // get a new process unique sequence number
                    long lSequenceNumber = Rmx::Profile::AppProfilerManager<ThreadProfiler_t >::GetAppProfiler()->GetNextSequenceNumber();
                    
                    // create a new profileevent log object (important point is this is created NOW- its elapsed time will be determined in the dtor)
                    m_pData = boost::shared_ptr<ProfileEventLogData> (new ProfileEventLogData(sName, m_ThreadProfiler.GetDepth(), lSequenceNumber, m_sLogType, m_spFluid));
                                                                                
                    // add log data to the log for this profiler
                    m_ThreadProfiler.LogStack().push_back(m_pData);
                    
                    // verify we are valid
                    assert(IsValid());
                }
                ~ProfileEvent() throw (Rmx::Profile::Errors::InternalError) // note- a dtor that throws- shared_ptrs are used, this may still leak- though if you get an exception here something REALLY bad happened
                {
                    // verify we are valid
                    assert(IsValid());
                    // Retrieve a reference to the log list
                    ThreadProfiler_t::ProfileEventLogDataContainerType & LogList = m_ThreadProfiler.LogStack();

                    // Retrieve a reference to the depth
                    int & iDepth = m_ThreadProfiler.GetDepth();
                    
                    // decrement the depth
                    iDepth--;
                    
                    // store the elapsed time
                    m_pData->SetElapsed(t.elapsed()) ;
                
                    // if we are the last object in this stack to be destroyed, so report all the logs
                    if (iDepth == 0)
                    {
                        ThreadProfiler_t::ProfileEventLogDataContainerType::iterator it;        
                        for (it = LogList.begin(); it != LogList.end(); it++)
                        {
                            boost::shared_ptr<ProfileEventLogData> pD = ((*it));
                            // iF we have a valid object
                            if (pD.get() != 0)
                            {
                                // call the policy handler on the ThreadProfiler to stream out the detail line
                                m_ThreadProfiler.ReportDetailLogLine(*(pD.get()));
                            }
                            else
                            {
                                assert(pD.get());
                                throw Rmx::Profile::Errors::InternalError(FLUID);
                            }
                        }
                        LogList.clear();
                    }
                }
        };
 
        // ScopedMeasurement
        // Just like an event- except what this does is  receieve notificaions from the 
        // any other profile event into its internal profiler (in addition to the thread profiler)
        // ScopedMeasurement is like a profile event
        // except we do mini profiling, any profile events created while we are alive               
        // also get reported to our internal profiler
        template <typename ThreadProfiler_t, typename Profile_ReportPolicy_t >
        class ScopedMeasurement:    public ProfileEvent<ThreadProfiler_t>,
                                    public Profile_ReportPolicy_t   
		{
		        detail::profile_manager_with_sink                   m_ScopedProfiler;          // Internal profiler for just the things that happen while we are alive
                typedef typename ThreadProfiler_t::duration_type    duration_type;

                // Generate a profile report for the Measured Scope
                // using the supplied policy class
                void GenerateProfileReport()    throw (Rmx::Profile::Errors::ProfileReportCreationFailureError)
                {
                    assert(IsValid() == true );

                    Profile_ReportPolicy_t::Report_StreamValueType * pOut = NULL;
                    AppProfiler<ThreadProfiler_t> * pAppProfiler =  Rmx::Profile::AppProfilerManager<ThreadProfiler_t >::GetAppProfiler();
                    // If we have an app profiler, then generate the report
                    if (pAppProfiler)
                    {
                        assert(pAppProfiler->DebugObjectValid() == true );

                        // get the report obj from the policy
                        pOut=  Profile_ReportPolicy_t::ProfileReportCreate(
                                                                            pAppProfiler->Settings.OutputDir(),
                                                                            pAppProfiler->Settings.ScopedReportFileName(),
                                                                            m_ThreadProfiler.ThreadName(),
                                                                            m_ThreadProfiler.ThreadId() 
                                                                           );
                        
                        // if one was received
                        if (pOut != NULL)
                        {
                            // Generate the header
                            Profile_ReportPolicy_t::ProfileReportStreamHeader(m_ThreadProfiler.ThreadLongName(), m_sName, *pOut);
                            // generate the report
                            m_ScopedProfiler.generate_report(*pOut); 

                            // Close the file
                            Profile_ReportPolicy_t::ProfileReportClose(*pOut);
                            pOut = NULL;            
                        }
                        else
                        {
                            // otherwise this is an error
                            throw Rmx::Profile::Errors::ProfileReportCreationFailureError(FLUID);
                        }
                   }
                }
           public:
                // Ctor
                // takes a thread profiler, event name, and a ptr to the fluid 
                ScopedMeasurement(ThreadProfiler_t & threadProfiler, std::string sName, boost::shared_ptr<Rmx::Debug::FileLineUniversalId> spFluid)
                : ProfileEvent<ThreadProfiler_t>(threadProfiler, sName, spFluid, "MS")
                {
                    assert(DebugObjectValid() == true );
		            // install our internal profiler to receieve notificaitons of profile events
		            m_ThreadProfiler.Add( &m_ScopedProfiler);
               }
                
                // dtor
                ~ScopedMeasurement()
                {
                    assert(DebugObjectValid() == true );
		            // UNinstall our internal profiler to receieve notificaitons of profile events
                    m_ThreadProfiler.Remove( &m_ScopedProfiler);
                    
                    // generate the profile report for this Measured Scope
                    GenerateProfileReport();
                }
        };
        
         // Implements (via RAII) Definition of a Profile Event   
        #define __RMX_PROFILE_EVENT_BEGIN_IMPL(NAME, THREAD_PROFILER_TYPE, LOG_TYPE)   \
    	              {  std::auto_ptr<Rmx::Profile::ProfileEvent<THREAD_PROFILER_TYPE > > p__profEvent##NAME;        \
	                    if (Rmx::Profile::AppProfilerManager<THREAD_PROFILER_TYPE>::GetAppProfilerIfEnabled() != NULL)  \
	                        p__profEvent##NAME = std::auto_ptr<Rmx::Profile::ProfileEvent<THREAD_PROFILER_TYPE > >     \
	                                                (new Rmx::Profile::ProfileEvent<THREAD_PROFILER_TYPE>(Rmx::Profile::AppProfilerManager<THREAD_PROFILER_TYPE >::GetAppProfiler()->GetThreadProfiler(),     \
	                                                                        #NAME, boost::shared_ptr<Rmx::Debug::FileLineUniversalId>(new FLUID), LOG_TYPE));      \
  
        #define __RMX_PROFILE_EVENT_END_IMPL(NAME, THREAD_PROFILER_TYPE, LOG_TYPE)   \
                      }                                                              \
        
        #define __RMX_PROFILE_EVENT_BEGIN(NAME, THREAD_PROFILER_TYPE)   \
                      __RMX_PROFILE_EVENT_BEGIN_IMPL(NAME, THREAD_PROFILER_TYPE,"PE");
 
         #define __RMX_PROFILE_EVENT_END(NAME, THREAD_PROFILER_TYPE)   \
                      __RMX_PROFILE_EVENT_END_IMPL(NAME, THREAD_PROFILER_TYPE,"PE");
                        
        // Implements (via RAII) Definition of a Measured Scope Profile Event
        // NOTE: Will add a "MS_" prepended to the name to denote a check point in the log
        #define __RMX_PROF_MEASURE_SCOPE_BEGIN_IMPL(NAME, THREAD_PROFILER_TYPE, MS_REPORT_POLICYTYPE)   \
    	            { std::auto_ptr<Rmx::Profile::ScopedMeasurement<THREAD_PROFILER_TYPE, MS_REPORT_POLICYTYPE > > p__profEvent##NAME;        \
	                if (Rmx::Profile::AppProfilerManager<THREAD_PROFILER_TYPE >::GetAppProfilerIfEnabled() != NULL)  \
	                    p__profEvent##NAME = std::auto_ptr<Rmx::Profile::ScopedMeasurement<THREAD_PROFILER_TYPE, MS_REPORT_POLICYTYPE > >     \
	                                                (new Rmx::Profile::ScopedMeasurement<THREAD_PROFILER_TYPE, MS_REPORT_POLICYTYPE > (Rmx::Profile::AppProfilerManager<THREAD_PROFILER_TYPE >::GetAppProfiler()->GetThreadProfiler(), \
	                                                                        "MS_"#NAME, boost::shared_ptr<Rmx::Debug::FileLineUniversalId>(new FLUID)));      \
        
        // Implements the END of Scope Measurement
        #define __RMX_PROF_MEASURE_SCOPE_END_IMPL(NAME, THREAD_PROFILER_TYPE, MS_REPORT_POLICYTYPE)    \
                    }
        
        // Reports calling threads profile report output stream mananged by the policy applied to the 
        // profiler
        // silently takes no action if profiling is disabled
        #define __RMX_REPORT_THREAD_PROFILE(THREAD_PROFILER_TYPE, NAME_OF_ACTION_REPORTING)             \
            if (Rmx::Profile::AppProfilerManager<THREAD_PROFILER_TYPE>::GetAppProfilerIfEnabled())                \
                Rmx::Profile::AppProfilerManager<THREAD_PROFILER_TYPE>::GetAppProfiler()->GetThreadProfiler().GenerateProfileReport(NAME_OF_ACTION_REPORTING);

        // Reports calling threads profile report to the passed ostream object
        // silently takes no action if profiling is disabled                                            
        #define __RMX_REPORT_THREAD_PROFILE_TO_STREAM(THREAD_PROFILER_TYPE, NAME_OF_ACTION_REPORTING, OUTPUTSTREAM)             \
            if (Rmx::Profile::AppProfilerManager<THREAD_PROFILER_TYPE>::GetAppProfilerIfEnabled())                \
                Rmx::Profile::AppProfilerManager<THREAD_PROFILER_TYPE>::GetAppProfiler()->GetThreadProfiler().GenerateProfileReport(NAME_OF_ACTION_REPORTING, OUTPUTSTREAM);


        #define __RMX_PROFILE_NAME_THREAD(THREAD_PROFILER_TYPE, NAME_OF_THREAD)                         \
        {                                                                                               \
            Rmx::Profile::AppProfiler<THREAD_PROFILER_TYPE > * pAppProf                                 \
                    = Rmx::Profile::AppProfilerManager<THREAD_PROFILER_TYPE>::GetAppProfiler();         \
            if (pAppProf)   pAppProf->DefineThread(NAME_OF_THREAD,FLUID);                               \
        } 
        
        // used by app code to MARK in the log a place where an error was caught
        // it does this by simply making a Profile Event
        // it prepends the text "ER_" to the passed name
        #define __RMX_PROFILE_LOG_MARK_ERROR(NAME, THREAD_PROFILER_TYPE)                                  \
        {    __RMX_PROFILE_EVENT_BEGIN_IMPL(ER_##NAME,THREAD_PROFILER_TYPE,"ER") __RMX_PROFILE_EVENT_END_IMPL(ER_##NAME,THREAD_PROFILER_TYPE,"ER")    }  \


    }; // namespace Profile
}; // namespace Rmx



#endif // __RMX__PROFILE_PROFILE_H


```
