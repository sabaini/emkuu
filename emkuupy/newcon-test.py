from emkuupy import connection, messages
import collections
import time

DATAFILE = "/home/sabaini/data/fh/da/dischargeletter-wo-chart.xml"
chartdata = "[\"TeleDiab_Course\", \"Timestamp1;BS;Timestamp2;BZ1_outlier;Timestamp3;BsLowerBorderValues;Timestamp4;BsUpperBorderValues\n04/04/2009 15:22:21;;04/04/2009;;04/04/2009;\n\", {\"NO_DATA_TEXT_COLOR\": \"#000000\", \"NO_DATA_TEXT\": \"Keine Daten verf\u00fcgbar\", \"axis_y_max\": \"330\", \"NO_DATA_TEXT_SIZE\": \"14\", \"legend_limit_low\": \"Untere Grenze\", \"xTitle\": \"\", \"CHART_HEIGHT\": \"284\", \"legend_series\": \"Blutzucker\", \"CHART_WIDTH\": \"595\", \"axis_y_min\": \"0\", \"legend_1\": \"Messwerte\", \"backcolor\": \"#ffE0E6EF\", \"axis_x_step\": \"7\", \"legend_median\": \"Median\", \"DATEAXIS_LEFTVALUE\": \"03/30/2009\", \"yTitle\": \"Blutzucker [mg/dl]\", \"DATEAXIS_RIGHTVALUE\": \"04/27/2009\", \"axis_x_label_format\": \"dd.MM\", \"legend_limit_high\": \"Obere Grenze\"}]"

def main():
    con = connection.EmkuuConnection(port=2350)
    data = open(DATAFILE).read().decode("utf-8")
    p = messages.PostMsg()
    p.fro = messages.Uri("/emkuu/newcon")
    p.to = messages.Uri("/emkuu/pdfgen")
    p.body = data
    r = messages.RouteMsg()
    r.fro = messages.Uri("/emkuu/newcon")
    r.to = messages.Uri("/emkuu/chartfx")
    r.body = chartdata
    r.enclosed = p
    con.send(r)
    con.close()
    #print "post msg: ", p.to_xml()

if __name__ == '__main__':
    main()

