from multiprocessing import Pool
import datetime
import subprocess

def run(date):
	subprocess.run(['Rscript', 'AveragingPolls.R', '../data', date.isoformat()])
	subprocess.run(['Rscript', 'Simulations.R', '../data', date.isoformat()])

if __name__ == '__main__':
	date = datetime.date(2020, 9, 1)
	stop = datetime.date.today() + datetime.timedelta(days=1)
	with Pool(48) as pool:
		pool.map(run, [date+datetime.timedelta(days) for days in range((stop-date).days)])
