from pypdf import PdfWriter, PdfReader
from pypdf.generic import NameObject

w = PdfWriter()
report = PdfReader("report.pdf")
w.append(report)
w.append(PdfReader("main.pdf"))
w._root_object.update({NameObject("/PageMode"): NameObject("/UseOutlines")})

if report.metadata is not None:
    w.add_metadata(report.metadata)


with open("total.pdf", "wb") as f:
    w.write(f)
print(f"total.pdf: {sum(1 for _ in PdfReader('total.pdf').pages)} pages")

