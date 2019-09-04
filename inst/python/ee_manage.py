def genreport():
    taks_list = []
    status=ee.data.getTaskList()
    for items in status:
        ttype=items['task_type']
        tdesc=items['description']
        tstate=items['state']
        tid=items['id']
        tcreate=datetime.datetime.fromtimestamp(items['creation_timestamp_ms']/1000).strftime('%Y-%m-%d %H:%M:%S')
        tstart=datetime.datetime.fromtimestamp(items['start_timestamp_ms']/1000).strftime('%Y-%m-%d %H:%M:%S')
        tupdate=datetime.datetime.fromtimestamp(items['update_timestamp_ms']/1000).strftime('%Y-%m-%d %H:%M:%S')
        tdiffstart=items['start_timestamp_ms']/1000-items['creation_timestamp_ms']/1000
        tdiffend=items['update_timestamp_ms']/1000-items['start_timestamp_ms']/1000
        try:
          error_message = items['error_message']
        except:
          error_message = "NULL"
        dict_summary = {
          'tid':tid,
          'tstate':tstate,
          'tdesc':tdesc,
          'ttype':ttype,
          'tcreate':tcreate,
          'tdiffstart':tdiffstart,
          'tdiffend':tdiffend,
          'error_message':error_message
          }
        taks_list.append(dict_summary)
    return taks_list
