    % for row in rows:
<%inherit file="base.html"/>

<%
    rows = [[v for v in range(0,10)] for row in range(0,10)]
%>
<%!
    rows = [[v for v in range(0,10)] for row in range(0,10)]
%>
aaa
<table>
  ## This is a comment.
    % for row in rows:
        ${makerow(row)}
    % endfor
</table>

<%def name="makerow(row)">
    <tr>
    % for name in row:
        <td>${name}</td>\
    % endfor
    </tr>
</%def>
<%doc>
This should be a comment too...
</%doc>
