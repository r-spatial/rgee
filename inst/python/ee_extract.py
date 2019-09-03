def table_format(table, rowId, colId,val):
    rows = table.distinct(rowId)
    joined = ee.Join.saveAll('matches').apply(
        primary =  rows,
        secondary = table,
        condition = ee.Filter.equals(
          leftField = rowId,
          rightField = rowId
        ))
    def get_f(row):
        values = ee.List(row.get('matches')).map(lambda feature: [
          ee.Feature(feature).get(colId),
          ee.Feature(feature).get(val)])
        return row.select([rowId]).set(ee.Dictionary(values.flatten()))
    return joined.map(get_f)
